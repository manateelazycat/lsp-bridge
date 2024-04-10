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

import json
import os
import queue
import re
import subprocess
import threading
import traceback
from subprocess import PIPE
from sys import stderr
from typing import TYPE_CHECKING, Dict
from urllib.parse import urlparse

from core.handler import Handler
from core.mergedeep import merge

if TYPE_CHECKING:
    from core.fileaction import FileAction
from core.utils import *

DEFAULT_BUFFER_SIZE = 100000000  # we need make buffer size big enough, avoid pipe hang by big data response from LSP server

INLAY_HINT_REQUEST_ID_DICT = {}

class LspServerSender(MessageSender):
    def __init__(self, process: subprocess.Popen, server_name, project_name):
        super().__init__(process)

        self.server_name = server_name
        self.project_name = project_name

        self.init_queue = queue.Queue()
        self.initialized = threading.Event()

    def enqueue_message(self, message: dict, *, init=False):
        message["jsonrpc"] = "2.0"
        if init:
            self.init_queue.put(message)
        else:
            self.queue.put(message)

    def send_request(self, method, params, request_id, **kwargs):
        self.enqueue_message(dict(
            id=request_id,
            method=method,
            params=params,
            message_type="request"
        ), **kwargs)

    def send_notification(self, method, params, **kwargs):
        self.enqueue_message(dict(
            method=method,
            params=params,
            message_type="notification"
        ), **kwargs)

    def send_response(self, request_id, result, **kwargs):
        self.enqueue_message(dict(
            id=request_id,
            result=result,
            message_type="response"
        ), **kwargs)

    def send_message(self, message: dict):
        json_content = json.dumps(message)

        message_str = "Content-Length: {}\r\n\r\n{}".format(len(json_content), json_content)

        self.process.stdin.write(message_str.encode("utf-8"))    # type: ignore
        self.process.stdin.flush()    # type: ignore

        # InlayHint will got error 'content modified' error if it followed immediately by a didChange request.
        # So we need INLAY_HINT_REQUEST_ID_DICT to contain documentation path to send retry request.
        record_inlay_hint_request(message)

        message_type = message.get("message_type")

        if message_type == "request" and \
           not message.get('method', 'response') == 'textDocument/documentSymbol':
            log_time("Send {} request ({}) to '{}' for project {}".format(
                message.get('method', 'response'),
                message.get('id', 'notification'),
                self.server_name,
                self.project_name
            ))
        elif message_type == "notification":
            log_time("Send {} notification to '{}' for project {}".format(
                message.get('method', 'response'),
                self.server_name,
                self.project_name
            ))
        elif message_type == "response":
            log_time("Send response to server request {} to '{}' for project {}".format(
                message.get('id', 'notification'),
                self.server_name,
                self.project_name
            ))

        logger.debug(json.dumps(message, indent=3))

    def run(self) -> None:
        try:
            # Send "initialize" request.
            self.send_message(self.init_queue.get())

            # Wait until initialized.
            self.initialized.wait()

            # Send other initialization-related messages.
            while not self.init_queue.empty():
                message = self.init_queue.get()
                self.send_message(message)

            # Send all others.
            while self.process.poll() is None:
                message = self.queue.get()
                self.send_message(message)
        except:
            logger.error(traceback.format_exc())

def record_inlay_hint_request(message):
    # InlayHint will got error 'content modified' error if it followed immediately by a didChange request.
    # So we need INLAY_HINT_REQUEST_ID_DICT to contain documentation path to send retry request.
    if message.get("method", "response") == "textDocument/inlayHint":
        try:
            message_id = message.get("id")
            message_documentation = message["params"]["textDocument"]["uri"]
            INLAY_HINT_REQUEST_ID_DICT[message_id] = message_documentation
        except:
            pass

def resend_inlay_hint_request_after_content_modified_error(message):
    # Get message file path.
    message_id = message.get("id")
    message_documentation = INLAY_HINT_REQUEST_ID_DICT[message_id]

    # Clean INLAY_HINT_REQUEST_ID_DICT to save memory.
    INLAY_HINT_REQUEST_ID_DICT.pop(message_id)

    # Call lsp-bridge-inlay-hint-retry to send retry request.
    eval_in_emacs("lsp-bridge-inlay-hint-retry", message_documentation)

class LspServerReceiver(MessageReceiver):

    def __init__(self, process: subprocess.Popen, server_name):
        super().__init__(process)

        self.server_name = server_name

    def emit_message(self, line):
        if not line:
            return
        try:
            # Send message.
            self.queue.put({
                "name": "lsp_recv_message",
                "content": parse_json_content(line)
            })
        except:
            logger.error(traceback.format_exc())

    def run(self):
        try:
            content_length = None
            buffer = bytearray()
            while self.process.poll() is None:
                if content_length is None:
                    match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                    if match is not None:
                        end = match.end()
                        parts = match.group(0).decode("utf-8").strip().split(": ")
                        content_length = int(parts[1])

                        buffer = buffer[end:]
                    else:
                        line = self.process.stdout.readline()    # type: ignore
                        # dart_analysis_server 会发送 Content-Type,
                        # 导致解析的 json 回包内容不完整
                        if re.search(b"Content-Type", line) is None:
                            buffer = buffer + line
                else:
                    if len(buffer) < content_length:
                        # 这个检查算是个防御吧，实际应该用不到了。先保留，后续再看。
                        match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                        if match is not None:
                            start = match.start()
                            msg = buffer[0:start]
                            buffer = buffer[start:]
                            content_length = None
                            self.emit_message(msg.decode("utf-8"))
                        else:
                            line = self.process.stdout.readline(content_length - len(buffer))    # type: ignore
                            buffer = buffer + line
                    else:
                        msg = buffer[0: content_length]
                        buffer = buffer[content_length:]
                        content_length = None
                        self.emit_message(msg.decode("utf-8"))
                if self.process.stderr:
                    logger.info(self.process.stderr.read())
            log_time("LSP server '{}' exited with code {}".format(self.server_name, self.process.returncode))
            logger.info(self.process.stdout.read())    # type: ignore
            if self.process.stderr:
                logger.info(self.process.stderr.read())
        except:
            logger.error(traceback.format_exc())

class LspServer:
    def __init__(self, message_queue, project_path, server_info, server_name, enable_diagnostics):
        self.message_queue = message_queue
        self.project_path = project_path
        self.project_name = os.path.basename(project_path)
        self.server_info = server_info

        self.initialize_id = generate_request_id()
        self.server_name = server_name
        self.enable_diagnostics = enable_diagnostics
        self.request_dict: Dict[int, Handler] = dict()
        self.root_path = self.project_path
        self.worksplace_folder = None

        # LSP server information.
        self.completion_trigger_characters = list()

        self.completion_resolve_provider = False
        self.rename_prepare_provider = False
        self.code_action_provider = False
        self.code_format_provider = False
        self.signature_help_provider = False
        self.workspace_symbol_provider = False
        self.inlay_hint_provider = False
        self.semantic_tokens_provider = False

        self.code_action_kinds = [
            "quickfix",
            "refactor",
            "refactor.extract",
            "refactor.inline",
            "refactor.rewrite",
            "source",
            "source.organizeImports"]
        self.text_document_sync = 2 # refer TextDocumentSyncKind. Can be None = 0, Full = 1 or Incremental = 2
        self.save_include_text = False

        # Start LSP server.
        self.lsp_subprocess = subprocess.Popen(self.server_info["command"],
                                               bufsize=DEFAULT_BUFFER_SIZE,
                                               stdin=PIPE,
                                               stdout=PIPE,
                                               stderr=stderr)

        # Two separate thread (read/write) to communicate with LSP server.
        self.receiver = LspServerReceiver(self.lsp_subprocess, self.server_info["name"])
        self.receiver.start()

        self.sender = LspServerSender(self.lsp_subprocess, self.server_info["name"], self.project_name)
        self.sender.start()

        # All LSP server response running in ls_message_thread.
        self.ls_message_thread = threading.Thread(target=self.lsp_message_dispatcher)
        self.ls_message_thread.start()

        self.files: Dict[str, "FileAction"] = dict()

    def attach(self, fa: "FileAction"):
        if is_in_path_dict(self.files, fa.filepath):
            logger.error(f"File {fa.filepath} opened again before close.")
            return

        add_to_path_dict(self.files, fa.filepath, fa)

        if len(self.files) == 1:
            # STEP 1: Say hello to LSP server.
            # Send 'initialize' request.
            self.send_initialize_request()

        # STEP 4: Tell LSP server open file.
        # We need send 'textDocument/didOpen' notification,
        # then LSP server will return file information, such as completion, find-define, find-references and rename etc.
        self.send_did_open_notification(fa)

    def lsp_message_dispatcher(self):
        try:
            while True:
                message = self.receiver.get_message()
                self.handle_recv_message(message["content"])
        except:
            logger.error(traceback.format_exc())

    def send_initialize_request(self):
        initialize_options = self.server_info.get("initializationOptions", {})

        self.worksplace_folder = get_emacs_func_result("get-workspace-folder", self.project_path)

        self.sender.send_request("initialize", {
            "processId": os.getpid(),
            "rootPath": self.root_path,
            "clientInfo": {
                "name": "emacs",
                "version": "lsp-bridge"
            },
            "rootUri": path_to_uri(self.project_path),
            "capabilities": self.get_capabilities(),
            "initializationOptions": initialize_options
        }, self.initialize_id, init=True)

    def get_capabilities(self):
        server_capabilities = self.server_info.get("capabilities", {})

        merge_capabilites = merge(server_capabilities, {
            "workspace": {
                "configuration": True,
                "symbol": {
                    "resolveSupport": {
                        "properties": []
                    }
                }
            },
            "textDocument": {
                "completion": {
                    "completionItem": {
                        "snippetSupport": True,
                        "deprecatedSupport": True,
                        "tagSupport": {
                            "valueSet": [
                                1
                            ]
                        },
                        "resolveSupport": {
                            # rust-analyzer need add `additionalTextEdits` to enable auto-import.
                            "properties": ["documentation", "detail", "additionalTextEdits"]
                        }
                    }
                },
                "codeAction": {
                    "dynamicRegistration": False,
                    "codeActionLiteralSupport": {
                        "codeActionKind": {
                            "valueSet": [
                                "quickfix",
                                "refactor",
                                "refactor.extract",
                                "refactor.inline",
                                "refactor.rewrite",
                                "source",
                                "source.organizeImports"
                            ]
                        }
                    },
                    "isPreferredSupport": True
                },
                "inlayHint": {
                    "dynamicRegistration": False
                }
            }
        })

        if isinstance(self.worksplace_folder, str):
            merge_capabilites = merge(merge_capabilites, {
                "workspace": {
                     "workspaceFolders": True
                }
            })

        merge_capabilites = merge(merge_capabilites, {
            "textDocument": {
                "publishDiagnostics": {
                    "relatedInformation": self.enable_diagnostics,
                    "tagSupport": {
                        "valueSet": [1, 2]
                    },
                    "versionSupport": self.enable_diagnostics,
                    "codeDescriptionSupport": self.enable_diagnostics,
                    "dataSupport": self.enable_diagnostics
                }
            }
        })

        return merge_capabilites

    def get_initialization_options(self):
        initialization_options = self.server_info.get("initializationOptions", {})

        if isinstance(self.worksplace_folder, str):
            initialization_options = merge(initialization_options, {
                "workspaceFolders": [
                    self.worksplace_folder
                ]
            })

        return initialization_options

    def parse_document_uri(self, filepath, external_file_link):
        """If FileAction include external_file_link return by LSP server, such as jdt.
        We should use external_file_link, such as uri 'jdt://xxx', otherwise use filepath as textDocument uri."""
        # Init with filepath.
        uri = path_to_uri(filepath)

        if external_file_link is not None:
            if urlparse(external_file_link).scheme != "":
                # Use external_file_link if we found scheme.
                uri = external_file_link
            else:
                # Otherwise external_file_link is filepath.
                uri = path_to_uri(external_file_link)

        return uri

    def get_language_id(self, fa):
        if "languageIds" in self.server_info:
            _, extension = os.path.splitext(fa.filepath)
            extension_name = extension.split(os.path.extsep)[-1]
            if extension_name in self.server_info["languageIds"]:
                return self.server_info["languageIds"][extension_name]

        return self.server_info["languageId"]

    def send_did_open_notification(self, fa: "FileAction"):
        self.sender.send_notification("textDocument/didOpen", {
            "textDocument": {
                "uri": self.parse_document_uri(fa.filepath, fa.external_file_link),
                "languageId": self.get_language_id(fa),
                "version": 0,
                "text": fa.read_file()
            }
        })

    def send_did_close_notification(self, filepath):
        self.sender.send_notification("textDocument/didClose", {
            "textDocument": {
                "uri": path_to_uri(filepath),
            }
        })

    def send_did_rename_files_notification(self, old_filepath, new_filepath):
        self.sender.send_notification("workspace/renameFiles", {
            "files": [{
                "oldUri": path_to_uri(old_filepath),
                "newUri": path_to_uri(new_filepath)
            }]
        })

    def send_did_save_notification(self, filepath, buffer_name):
        args = {
            "textDocument": {
                "uri": path_to_uri(filepath)
            }
        }

        # Fetch buffer whole content to LSP server if server capability 'includeText' is True.
        if self.save_include_text:
            args = merge(args, {
                "textDocument": {
                    "text": get_buffer_content(filepath, buffer_name)
                }
            })

        self.sender.send_notification("textDocument/didSave", args)

    def send_did_change_notification(self, filepath, version, start, end, range_length, text):
        # STEP 5: Tell LSP server file content is changed.
        # This step is very IMPORTANT, make sure LSP server contain same content as client,
        # otherwise LSP server won't response client request, such as completion, find-define, find-references and rename etc.
        self.sender.send_notification("textDocument/didChange", {
            "textDocument": {
                "uri": path_to_uri(filepath),
                "version": version
            },
            "contentChanges": [
                {
                    "range": {
                        "start": start,
                        "end": end
                    },
                    "rangeLength": range_length,
                    "text": text
                }
            ]
        })

    def send_whole_change_notification(self, filepath, version, file_content=None):
        if not file_content:
            with open(filepath, encoding="utf-8", errors="ignore") as f:
                file_content = f.read()
        self.sender.send_notification("textDocument/didChange", {
            "textDocument": {
                "uri": path_to_uri(filepath),
                "version": version
            },
            "contentChanges": [
                {
                    "text": file_content
                }
            ]
        })

    def record_request_id(self, request_id: int, handler: Handler):
        self.request_dict[request_id] = handler

    def send_shutdown_request(self):
        self.sender.send_request("shutdown", {}, generate_request_id())

    def send_exit_notification(self):
        self.sender.send_notification("exit", {})

    def get_server_workspace_change_configuration(self):
        return {
            "settings": self.server_info.get("settings", {})
        }

    def handle_workspace_configuration_request(self, name, request_id, params):
        settings = self.server_info.get("settings", {})

        # NOTE: We send message fill null with same length of workspace/configuration params and send back to server
        # if nothing in 'settings' of server.json file.
        # Otherwise, some LSP server, such as zls will crash if we just send back empty list.
        if settings is None or len(settings) == 0:
            self.sender.send_response(request_id, [None] * len(params["items"]))
            return

        # Otherwise, send back section value or default settings.
        items = []
        for p in params["items"]:
            section = p.get("section", self.server_info["name"])
            sessionSettings = settings.get(section, {})

            if self.server_info["name"] == "vscode-eslint-language-server":
                sessionSettings = settings
                sessionSettings["workspaceFolder"] = {
                    "name": self.project_name,
                    "uri": path_to_uri(self.project_path),
                }

            items.append(sessionSettings)
        self.sender.send_response(request_id, items)

    def handle_error_message(self, message):
        logger.error("Recv message (error):")
        logger.error(json.dumps(message, indent=3))

        # InlayHint will got error 'content modified' error if it followed immediately by a didChange request.
        # If we found this error, call lsp-bridge-inlay-hint-retry to send retry request.
        if "id" in message:
            message_id = message.get("id")
            if message_id in INLAY_HINT_REQUEST_ID_DICT:
                resend_inlay_hint_request_after_content_modified_error(message)
                return

        error_message = message["error"]["message"]
        provider_attributes = {
            "Unhandled method completionItem/resolve": "completion_resolve_provider",
            "Unhandled method textDocument/prepareRename": "rename_prepare_provider",
            "Unhandled method textDocument/codeAction": "code_action_provider",
            "Unhandled method textDocument/formatting": "code_format_provider",
            "Unhandled method textDocument/signatureHelp": "signature_help_provider",
            "Unhandled method workspace/symbol": "workspace_symbol_provider",
            "Unhandled method textDocument/inlayHint": "inlay_hint_provider",
        }

        if error_message in provider_attributes:
            setattr(self, provider_attributes[error_message], False)
        else:
            message_emacs(error_message)

    def record_message(self, message):
        if "id" in message:
            if "method" in message:
                # server request
                log_time("Recv {} request ({}) from '{}' for project {}".format(message["method"], message["id"], self.server_info["name"], self.project_name))
            else:
                # server response
                if message["id"] in self.request_dict:
                    method = self.request_dict[message["id"]].method
                    if method != 'textDocument/documentSymbol':
                        # not log for textDocument/documentSymbol
                        log_time("Recv {} response ({}) from '{}' for project {}".format(method, message["id"], self.server_info["name"], self.project_name))
                else:
                    log_time("Recv response ({}) from '{}' for project {}".format(message["id"], self.server_info["name"], self.project_name))
        else:
            if "method" in message:
                # server notification
                log_time("Recv {} notification from '{}' for project {}".format(message["method"], self.server_info["name"], self.project_name))
            else:
                # others
                log_time("Recv message {} from '{}' for project {}".format(message, self.server_info["name"], self.project_name))

    def handle_diagnostics_message(self, message):
        self.handle_publish_diagnostics(message)

        self.handle_dart_publish_closing_labels(message)

    def handle_publish_diagnostics(self, message):
        if "method" in message and message["method"] == "textDocument/publishDiagnostics":
            filepath = uri_to_path(message["params"]["uri"])
            if self.enable_diagnostics and is_in_path_dict(self.files, filepath):
                get_from_path_dict(self.files, filepath).record_diagnostics(message["params"]["diagnostics"], self.server_info["name"])

    def handle_dart_publish_closing_labels(self, message):
        if "method" in message and message["method"] == "dart/textDocument/publishClosingLabels":
            filepath = uri_to_path(message["params"]["uri"])
            if is_in_path_dict(self.files, filepath):
                get_from_path_dict(self.files, filepath).record_dart_closing_lables(message["params"]["labels"])

    def handle_log_message(self, message):
        # Notice user if got error message from lsp server.
        if "method" in message and message["method"] == "window/logMessage":
            try:
                if "error" in message["params"]["message"].lower():
                    message_emacs("{} ({}): {}".format(self.project_name, self.server_info["name"], message["params"]["message"]))
            except:
                pass

    def set_attribute_from_message(self, message, attribute_name, key_list):
        def get_nested_value(dct, keys):
            for key in keys:
                try:
                    dct = dct[key]
                except (KeyError, TypeError):
                    return None
            return dct

        value = get_nested_value(message, key_list)
        if value is not None:
            setattr(self, attribute_name, value)

    def save_attribute_from_message(self, message):
        attributes_to_set = [
            ("completion_trigger_characters", ["result", "capabilities", "completionProvider", "triggerCharacters"]),
            ("completion_resolve_provider", ["result", "capabilities", "completionProvider", "resolveProvider"]),
            ("rename_prepare_provider", ["result", "capabilities", "renameProvider", "prepareProvider"]),
            ("code_action_provider", ["result", "capabilities", "codeActionProvider"]),
            ("code_action_kinds", ["result", "capabilities", "codeActionProvider", "codeActionKinds"]),
            ("code_format_provider", ["result", "capabilities", "documentFormattingProvider"]),
            ("signature_help_provider", ["result", "capabilities", "signatureHelpProvider"]),
            ("workspace_symbol_provider", ["result", "capabilities", "workspaceSymbolProvider"]),
            ("inlay_hint_provider", ["result", "capabilities", "inlayHintProvider", "resolveProvider"]),
            ("save_include_text", ["result", "capabilities", "textDocumentSync", "save", "includeText"]),
            ("text_document_sync", ["result", "capabilities", "textDocumentSync"]),
            ("semantic_tokens_provider", ["result", "capabilities", "semanticTokensProvider"])]

        for attr, path in attributes_to_set:
            self.set_attribute_from_message(message, attr, path)

        if isinstance(self.text_document_sync, dict):
            self.text_document_sync = self.text_document_sync.get("change", self.text_document_sync)

        # Some LSP server has inlayHint capability, but won't response inlayHintProvider in capability message.
        # So we set `inlay_hint_provider` to True if found `forceInlayHint` option in config file.
        if "forceInlayHint" in self.server_info and self.server_info["forceInlayHint"] is True:
            self.inlay_hint_provider = True

    def send_initialize_response(self, message):
        self.sender.send_notification("initialized", {}, init=True)

        self.sender.send_notification("workspace/didChangeConfiguration", self.get_server_workspace_change_configuration(), init=True)

        self.sender.initialized.set()

    def handle_workspace_message(self, message):
        if message["method"] == "workspace/configuration":
            self.handle_workspace_configuration_request(message["method"], message["id"], message["params"])
        elif message["method"] == "workspace/applyEdit":
            eval_in_emacs("lsp-bridge-workspace-apply-edit", message["params"]["edit"])
            self.sender.send_response(message["id"], { "applied": True })

    def handle_id_message(self, message):
        if "id" in message:
            if message["id"] == self.initialize_id:
                # STEP 2: tell LSP server that client is ready.
                # We need wait LSP server response 'initialize', then we send 'initialized' notification.
                self.save_attribute_from_message(message)

                # STEP 3: Configure LSP server parameters.
                # After 'initialized' message finish, we should send 'workspace/didChangeConfiguration' notification.
                # The setting parameters of each language server are different.
                self.send_initialize_response(message)
            else:
                if "method" not in message and message["id"] in self.request_dict:
                    handler = self.request_dict[message["id"]]
                    handler.handle_response(
                        request_id=message["id"],
                        response=message["result"],
                    )
                else:
                    self.handle_workspace_message(message)

    def handle_recv_message(self, message: dict):
        if "error" in message:
            self.handle_error_message(message)
            return

        self.record_message(message)
        self.handle_diagnostics_message(message)
        self.handle_log_message(message)
        self.handle_id_message(message)

        logger.debug(json.dumps(message, indent=3))

    def close_file(self, filepath):
        # Send didClose notification when client close file.
        if is_in_path_dict(self.files, filepath):
            self.send_did_close_notification(filepath)
            remove_from_path_dict(self.files, filepath)

        # We need shutdown LSP server when last file closed, to save system memory.
        if len(self.files) == 0:
            self.message_queue.put({
                "name": "server_process_exit",
                "content": self.server_name
            })
            self.exit()

    def exit(self):
        self.send_shutdown_request()
        self.send_exit_notification()
        # Don't need to wait LSP server response, kill immediately.
        if self.lsp_subprocess is not None:
            try:
                os.kill(self.lsp_subprocess.pid, 9)
            except ProcessLookupError:
                log_time("LSP server {} ({}) already exited!".format(self.server_info["name"], self.lsp_subprocess.pid))
