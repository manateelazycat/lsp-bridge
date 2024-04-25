#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import os
import pprint
import threading
import time
from typing import TYPE_CHECKING, Dict, Tuple

from core.handler import *
from core.lspserver import LspServer
from core.utils import *

if TYPE_CHECKING:
    from lsp_bridge import LspBridge  # noqa: F401

def create_file_action_with_single_server(filepath, single_server_info, single_server, external_file_link=None):
    if is_in_path_dict(FILE_ACTION_DICT, filepath):
        if get_from_path_dict(FILE_ACTION_DICT, filepath).single_server != single_server:
            logger.warn("File {} is opened by different lsp server.".format(filepath))
        return

    action = FileAction(filepath, single_server_info, single_server, None, None, external_file_link)
    add_to_path_dict(FILE_ACTION_DICT, filepath, action)
    return action

def create_file_action_with_multi_servers(filepath, multi_servers_info, multi_servers, external_file_link=None):
    action = FileAction(filepath, None, None, multi_servers_info, multi_servers, external_file_link)
    add_to_path_dict(FILE_ACTION_DICT, filepath, action)
    return action

class FileAction:
    def __init__(self, filepath, single_server_info, single_server, multi_servers_info, multi_servers, external_file_link):
        # Init.
        self.single_server_info = single_server_info
        self.single_server: LspServer = single_server
        self.multi_servers = multi_servers
        self.multi_servers_info = multi_servers_info

        self.code_actions = {}
        self.code_action_counter = 0

        self.completion_item_resolve_key = None
        self.completion_items = {}

        self.diagnostics = {}
        self.diagnostics_ticker = {}

        self.external_file_link = external_file_link
        self.filepath = filepath

        self.last_change_cursor_time = -1.0
        self.last_change_file_time = -1.0

        self.request_dict = {}

        self.try_completion_timer = None

        self.version = 1

        self.org_file = os.path.splitext(filepath)[-1] == '.org'
        self.org_line_bias = None

        # We need multiple servers to handle org files
        self.org_lang_servers = {}
        self.org_server_infos = {}
        if self.org_file:
             self.org_lang_servers[self.single_server.server_name] = self.single_server
             self.org_server_infos[self.single_server.server_name] = self.single_server_info

        # Initialize handlers.
        self.handlers: Dict[str, Handler] = dict()
        logger.debug("Handlers: " + pprint.pformat(Handler.__subclasses__()))
        for handler_cls in Handler.__subclasses__():
            self.handlers[handler_cls.name] = handler_cls(self)

        (self.enable_auto_import,
         self.completion_items_limit,
         self.completion_match_mode,
         self.insert_spaces,
         self.enable_push_diagnostics,
         self.push_diagnostic_idle,
         self.display_label_max_length,
         self.diagnostics_max_number) = get_emacs_vars([
             "acm-backend-lsp-enable-auto-import",
             "acm-backend-lsp-candidates-max-number",
             "acm-backend-lsp-match-mode",
             "indent-tabs-mode",
             "lsp-bridge-enable-diagnostics",
             "lsp-bridge-diagnostic-fetch-idle",
             "acm-backend-lsp-candidate-max-length",
             "lsp-bridge-diagnostic-max-number"
        ])
        self.completion_block_kind_list = None
        self.insert_spaces = not self.insert_spaces

        self.set_lsp_server()

    def set_lsp_server(self):
        """Set LSP handlers, prefix and name """
        self.method_handlers = {}
        for lsp_server in self.get_lsp_servers():
            method_handlers_dict = {}
            for handler_cls in Handler.__subclasses__():
                method_handlers_dict[handler_cls.name] = handler_cls(self)

            self.method_handlers[lsp_server.server_info["name"]] = method_handlers_dict

            lsp_server.attach(self)

        # Set acm-input-bound-style when opened file.
        if self.single_server_info is not None:
            eval_in_emacs("lsp-bridge-set-prefix-style", self.single_server_info.get("prefixStyle", "ascii"))

        # Init server names.
        eval_in_emacs("lsp-bridge-set-server-names", self.filepath, get_lsp_file_host(), self.get_lsp_server_names())

    @property
    def last_change(self) -> Tuple[float, float]:
        """Return the last change information as a tuple."""
        return self.last_change_file_time, self.last_change_cursor_time

    def read_file(self):
        """Read file content."""
        file_content = ''
        if self.org_file:
            file_content = get_buffer_content(self.filepath, os.path.basename(self.filepath))
        else:
            with open(self.filepath, encoding="utf-8", errors="ignore") as f:
                file_content = f.read()
        return file_content

    def call(self, method, *args, **kwargs):
        """Call any handler or method of file action."""
        if method in self.handlers:
            handler = self.handlers[method]
            if self.single_server:
                self.send_request(self.single_server, method, handler, *args, **kwargs)
            else:
                if method in ["completion", "completion_item_resolve", "diagnostics", "code_action", "execute_command"]:
                    method_server_names = self.multi_servers_info[method]
                else:
                    method_server_names = [self.multi_servers_info[method]]

                for method_server_name in method_server_names:
                    method_server = self.multi_servers[method_server_name]
                    self.send_request(method_server, method, handler, *args, **kwargs)
        elif hasattr(self, method):
            getattr(self, method)(*args, **kwargs)

    def send_request(self, method_server, method, handler, *args, **kwargs):
        if hasattr(handler, "provider"):
            if getattr(method_server, getattr(handler, "provider")):
                self.send_server_request(method_server, method, *args, **kwargs)
            elif hasattr(handler, "provider_message"):
                message_emacs(getattr(handler, "provider_message"))
        else:
            self.send_server_request(method_server, method, *args, **kwargs)

    def change_file(self, start, end, range_length, change_text, position, before_char, buffer_name, prefix):
        if self.org_file:
            if self.org_line_bias is None:
                return
            start['line'] -= self.org_line_bias
            end['line'] -= self.org_line_bias

        buffer_content = ''
        # Send didChange request to LSP server.
        for lsp_server in self.get_lsp_servers():
            if lsp_server.text_document_sync == 0:
                continue
            elif lsp_server.text_document_sync == 1:
                if not buffer_content:
                    buffer_content = get_buffer_content(self.filepath, buffer_name)
                lsp_server.send_whole_change_notification(self.filepath, self.version, buffer_content)
            else:
                lsp_server.send_did_change_notification(self.filepath, self.version, start, end, range_length, change_text)

        self.version += 1

        # Try cancel expired completion timer.
        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        # Record last change information.
        self.last_change_file_time = time.time()

        # Send textDocument/completion 100ms later.
        if self.completion_block_kind_list is None:
            (self.completion_block_kind_list, ) = get_emacs_vars(["acm-backend-lsp-block-kind-list"])
            if isinstance(self.completion_block_kind_list, list):
                self.completion_block_kind_list = list(map(lambda x: x.lower(), self.completion_block_kind_list))

        delay = 0 if is_running_in_server() else 0.1
        self.try_completion_timer = threading.Timer(delay, lambda : self.try_completion(position, before_char, prefix, self.version))
        self.try_completion_timer.start()

    def update_file(self, buffer_name, org_line_bias=None):
        self.org_line_bias = org_line_bias
        buffer_content = get_buffer_content(self.filepath, buffer_name)
        for lsp_server in self.get_lsp_servers():
            lsp_server.send_whole_change_notification(self.filepath, self.version, buffer_content)
        self.version += 1

    def try_completion(self, position, before_char, prefix, version=None):
        # If we call try_completion from Elisp side, Emacs don't know the version of FileAction.
        # So we need fill version if it is None.
        if self.org_file:
            # update line bias in `try_completion` instead of `change_file`
            # to avoid mismatch when `try_completion` is called directly through `lsp-bridge-popup-complete-menu`
            if self.org_line_bias is None:
                return
            position['line'] -= self.org_line_bias

        if version is None:
            version = self.version

        if self.multi_servers:
            for lsp_server in self.multi_servers.values():
                if lsp_server.server_info["name"] in self.multi_servers_info["completion"]:
                    self.send_server_request(lsp_server, "completion", lsp_server, position, before_char, prefix, version)
        else:
            self.send_server_request(self.single_server, "completion", self.single_server, position, before_char, prefix, version)

    def try_formatting(self, start, end, *args, **kwargs):
        if self.multi_servers:
            for lsp_server in self.multi_servers.values():
                if lsp_server.server_info["name"] in self.multi_servers_info["formatting"]:
                    if start == end:
                        self.send_request(lsp_server, "formatting", Formatting, *args, **kwargs)
                    else:
                        self.send_request(lsp_server, "rangeFormatting", RangeFormatting, start, end, *args, **kwargs)
        else:
            if start == end:
                self.send_request(self.single_server, "formatting", Formatting, *args, **kwargs)
            else:
                self.send_request(self.single_server, "rangeFormatting", RangeFormatting, start, end, *args, **kwargs)

    def try_code_action(self, *args, **kwargs):
        self.code_action_counter = 0

        if self.multi_servers:
            for lsp_server in self.multi_servers.values():
                if lsp_server.server_info["name"] in self.multi_servers_info["code_action"]:
                    self.send_code_action_request(lsp_server, *args, **kwargs)
        else:
            self.send_code_action_request(self.single_server, *args, **kwargs)

    def change_cursor(self, position):
        # Record change cursor time.
        self.last_change_cursor_time = time.time()

    def get_diagnostics_count(self):
        return sum(len(diags) for diags in self.diagnostics.values())

    def get_diagnostics(self):
        diagnostics = []
        diagnostic_count = 0
        for server_name in self.diagnostics:
            for diagnostic in self.diagnostics[server_name]:
                diagnostic["server-name"] = server_name
                diagnostics.append(diagnostic)

                diagnostic_count += 1

                if diagnostic_count >= self.diagnostics_max_number:
                    return diagnostics

        return diagnostics

    def list_diagnostics(self):
        diagnostic_count = 0
        for server_name in self.diagnostics:
            diagnostic_count += len(self.diagnostics[server_name])

        if diagnostic_count == 0:
            message_emacs("No diagnostics found.")
        else:
            eval_in_emacs("lsp-bridge-diagnostic--list", self.get_diagnostics())

    def sort_diagnostic(self, diagnostic_a, diagnostic_b):
        score_a = [diagnostic_a["range"]["start"]["line"],
                   diagnostic_a["range"]["start"]["character"],
                   diagnostic_a["range"]["end"]["line"],
                   diagnostic_a["range"]["end"]["character"]]
        score_b = [diagnostic_b["range"]["start"]["line"],
                   diagnostic_b["range"]["start"]["character"],
                   diagnostic_b["range"]["end"]["line"],
                   diagnostic_b["range"]["end"]["character"]]

        if score_a < score_b:
            return -1
        elif score_a > score_b:
            return 1
        else:
            return 0

    def record_diagnostics(self, diagnostics, server_name):
        log_time("Record diagnostics from '{}' for file {}".format(server_name, os.path.basename(self.filepath)))

        # Record diagnostics data that push from LSP server.
        import functools
        self.diagnostics[server_name] = sorted(diagnostics, key=functools.cmp_to_key(self.sort_diagnostic))

        if server_name in self.diagnostics_ticker:
            self.diagnostics_ticker[server_name] += 1
        else:
            self.diagnostics_ticker[server_name] = 0

        # Try to push diagnostics to Emacs.
        if self.enable_push_diagnostics:
            push_diagnostic_ticker = self.diagnostics_ticker[server_name]
            push_diagnostic_timer = threading.Timer(
                self.push_diagnostic_idle,
                lambda : self.try_push_diagnostics(push_diagnostic_ticker, server_name))
            push_diagnostic_timer.start()

    def try_push_diagnostics(self, ticker, server_name):
        # Only push diagnostics to Emacs when ticker is newest.
        # Drop all temporarily diagnostics when typing.
        #
        # Note:
        # We need to check diagnostics ticker separately based on LSP server name,
        # to avoid multiple LSP server conflict each other.
        if ticker == self.diagnostics_ticker[server_name]:
            eval_in_emacs("lsp-bridge-diagnostic--render",
                          self.filepath,
                          get_lsp_file_host(),
                          self.get_diagnostics(),
                          self.get_diagnostics_count())

    def record_dart_closing_lables(self, labels):
        eval_in_emacs("lsp-bridge-dart-closing-labels--render", self.filepath, get_lsp_file_host(), labels)

    def push_code_actions(self, actions, server_name, action_kind):
        log_time("Record actions from '{}' for file {}".format(server_name, os.path.basename(self.filepath)))
        self.code_actions[server_name] = actions

        self.code_action_counter += 1
        check_counter = len(self.multi_servers_info.get("code_action", [])) if self.multi_servers else 1

        # Only send code action when all LSP server has received response.
        if self.code_action_counter >= check_counter:
            self.code_action_counter = 0

            code_actions = self.get_code_actions()
            if len(code_actions) > 0:
                eval_in_emacs("lsp-bridge-code-action--fix", self.get_code_actions(), action_kind)
            elif self.get_diagnostics_count() > 0:
                message_emacs("Please move cursor to error or warning, then execute 'lsp-bridge-code-action' again.")
            else:
                message_emacs("Fantastic, your code looks great! No further actions needed!")

    def get_code_actions(self):
        code_actions = []
        for server_name in self.code_actions:
            if self.code_actions[server_name] is not None:
                for code_action in self.code_actions[server_name]:
                    code_action["server-name"] = server_name
                    code_actions.append(code_action)

        return code_actions

    def send_code_action_request(self, lsp_server, range_start, range_end, action_kind):
        lsp_server_name = lsp_server.server_info["name"]

        diagnostics = []
        if lsp_server_name in self.diagnostics:
            for diagnostic in self.diagnostics[lsp_server_name]:
                if "range" in diagnostic and not (
                    diagnostic["range"]["start"]["line"] >= range_start["line"]
                    and diagnostic["range"]["end"]["line"] <= range_end["line"]
                ):
                    continue
                diagnostics.append(diagnostic)

        self.send_request(
            lsp_server, "code_action", Codeaction,
            lsp_server_name, diagnostics, range_start, range_end, action_kind
        )

    def save_file(self, buffer_name):
        for lsp_server in self.get_lsp_servers():
            lsp_server.send_did_save_notification(self.filepath, buffer_name)

    def completion_item_resolve(self, item_key, server_name):
        if server_name in self.completion_items:
            self.completion_item_resolve_key = item_key

            if item_key in self.completion_items[server_name]:

                if self.multi_servers:
                    method_server = self.multi_servers[server_name]
                else:
                    method_server = self.single_server

                if method_server.completion_resolve_provider:
                    self.send_server_request(method_server,
                                             "completion_item_resolve",
                                             item_key,
                                             server_name,
                                             self.completion_items[server_name][item_key])
                else:
                    item = self.completion_items[server_name][item_key]

                    self.completion_item_update(
                        item_key,
                        server_name,
                        item["documentation"] if "documentation" in item else "",
                        item["additionalTextEdits"] if "additionalTextEdits" in item else "")
            else:
                # Still update completion documentation/additionalTextEdits with empty if item not in completion_items.
                self.completion_item_update(item_key, server_name, "", "")

    def completion_item_update(self, item_key, server_name, documentation, additional_text_edits):
        if self.completion_item_resolve_key == item_key:
           if isinstance(documentation, dict):
               if "kind" in documentation:
                   documentation = documentation["value"]

           eval_in_emacs("lsp-bridge-completion-item--update",
                         {
                             "filepath": self.filepath,
                             "key": item_key,
                             "server": server_name,
                             "additionalTextEdits": additional_text_edits,
                             "documentation": documentation
                         },
                         get_lsp_file_host())


    def rename_file(self, old_filepath, new_filepath):
        self.get_lsp_servers()[0].send_did_rename_files_notification(old_filepath, new_filepath)

    def send_server_request(self, lsp_server, handler_name, *args, **kwargs):
        handler: Handler = self.method_handlers[lsp_server.server_info["name"]][handler_name]

        handler.latest_request_id = request_id = generate_request_id()
        handler.last_change = self.last_change

        lsp_server.record_request_id(request_id, handler)

        params = handler.process_request(*args, **kwargs)
        if handler.send_document_uri:
            params["textDocument"] = {"uri": lsp_server.parse_document_uri(self.filepath, self.external_file_link)}

        lsp_server.sender.send_request(
            method=handler.method,
            params=params,
            request_id=request_id)

    def exit(self):
        for lsp_server in (self.org_lang_servers.values() if self.org_file else self.get_lsp_servers()):
            if lsp_server.server_name in LSP_SERVER_DICT:
                lsp_server = LSP_SERVER_DICT[lsp_server.server_name]
                lsp_server.close_file(self.filepath)

        # Clean FILE_ACTION_DICT after close file.
        remove_from_path_dict(FILE_ACTION_DICT, self.filepath)

    def get_lsp_servers(self):
        return self.multi_servers.values() if self.multi_servers else [self.single_server]

    def get_lsp_server_names(self):
        return list(map(lambda lsp_server: lsp_server.server_info["name"], self.get_lsp_servers()))

    def get_lsp_server_project_path(self):
        return self.single_server.project_path.encode('utf-8')

    def get_match_lsp_servers(self, method):
        if self.multi_servers:
            server_names = self.multi_servers_info[method]    # type: ignore
            if isinstance(server_names, str):
                server_names = [server_names]

            return list(map(lambda server_name: self.multi_servers[server_name], server_names))    # type: ignore
        else:
            return [self.single_server]

    def create_external_file_action(self, external_file, external_file_link=None):
        if self.multi_servers:
            create_file_action_with_multi_servers(external_file, self.multi_servers_info, self.multi_servers, external_file_link)
        else:
            create_file_action_with_single_server(external_file, self.single_server_info, self.single_server, external_file_link)

FILE_ACTION_DICT: Dict[str, FileAction] = {}  # use for contain file action
LSP_SERVER_DICT: Dict[str, LspServer] = {}  # use for contain lsp server
