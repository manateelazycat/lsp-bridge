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
import queue
import sys
import threading
from collections import defaultdict
from typing import Dict

from epc.server import ThreadingEPCServer

from core.fileaction import FileAction
from core.lspserver import LspServer
from core.utils import *
from core.handler import *


class LspBridge:
    def __init__(self, args):

        # Object cache to exchange information between Emacs and LSP server.
        self.file_action_dict: Dict[str, FileAction] = {}  # use for contain file action
        self.lsp_server_dict: Dict[str, LspServer] = {}  # use for contain lsp server
        self.file_opened: Dict[str, bool] = defaultdict(bool)
        self.action_cache_dict: Dict[str, list] = defaultdict(list)  # use for contain file action cache

        # Build EPC interfaces.
        for name in ["change_file", "find_define", "find_implementation", "find_references",
                     "prepare_rename", "rename", "change_cursor", "save_file", "hover", "signature_help"]:
            self.build_file_action_function(name)

        for cls in Handler.__subclasses__():
            self.build_file_action_function(cls.name)

        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(lsp-bridge_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Init emacs option.
        enable_lsp_server_log = get_emacs_var("lsp-bridge-enable-log")
        if enable_lsp_server_log:
            logger.setLevel(logging.DEBUG)

        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # All LSP server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        # Pass epc port and webengine codec information to Emacs when first start LspBridge.
        eval_in_emacs('lsp-bridge--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        while True:
            message = self.event_queue.get(True)

            if message["name"] == "open_file":
                self._open_file(message["content"])
            elif message["name"] == "close_file":
                self._close_file(message["content"])
            elif message["name"] == "action_func":
                (func_name, func_args) = message["content"]
                getattr(self, func_name)(*func_args)

            self.event_queue.task_done()

    def message_dispatcher(self):
        while True:
            message = self.message_queue.get(True)
            if message["name"] == "server_file_opened":
                self.handle_server_file_opened(message["content"])
            elif message["name"] == "server_response_message":
                self.handle_server_message(*message["content"])
            elif message["name"] == "make_lsp_location_link_file_action":
                filepath = message["content"]["filepath"]
                file_action = message["content"]["file_action"]
                self.file_action_dict[path_as_key(filepath)] = file_action

            self.message_queue.task_done()

    def open_file(self, filepath):
        # We need post function event_loop, otherwise long-time calculation will block Emacs.
        self.event_queue.put({
            "name": "open_file",
            "content": filepath
        })

    def _open_file(self, filepath):
        # Create file action.
        file_key = path_as_key(filepath)
        if file_key not in self.file_action_dict:
            project_path = get_project_path(filepath)
            lang_server = get_emacs_func_result("get-lang-server", project_path, filepath)
            action = FileAction(filepath, project_path, lang_server)
            self.file_action_dict[file_key] = action

        # Create LSP server.
        file_action = self.file_action_dict[file_key]
        lsp_server_name = file_action.get_lsp_server_name()
        if lsp_server_name not in self.lsp_server_dict:
            # lsp server will send initialize and didOpen when open first file in project.
            server = LspServer(self.message_queue, file_action, self.lsp_server_dict)
            self.lsp_server_dict[lsp_server_name] = server
        else:
            # Send didOpen notification to LSP server.
            self.lsp_server_dict[lsp_server_name].send_did_open_notification(file_action.filepath, file_action.lsp_location_link)

        # Add lsp server in file action for send message to lsp server.
        file_action.lsp_server = self.lsp_server_dict[lsp_server_name]

    def close_file(self, filepath):
        # We need post function event_loop, otherwise long-time calculation will block Emacs.
        self.event_queue.put({
            "name": "close_file",
            "content": filepath
        })

    def _close_file(self, filepath):
        file_key = path_as_key(filepath)
        if file_key in self.file_action_dict:
            action = self.file_action_dict[file_key]

            lsp_server_name = action.get_lsp_server_name()
            if lsp_server_name in self.lsp_server_dict:
                lsp_server = self.lsp_server_dict[lsp_server_name]
                lsp_server.close_file(filepath)

            # Clean file_action_dict and file_opened after close file.
            del self.file_action_dict[file_key]
            del self.file_opened[file_key]

    def build_file_action_function(self, name):
        def _do(filepath, *args):
            file_key = path_as_key(filepath)
            if self.file_opened[file_key]:
                action = self.file_action_dict[file_key]
                action.call(name, *args)
            else:
                if file_key not in self.file_action_dict:
                    self._open_file(filepath)  # _do is called inside event_loop, so we can block here.

                # Cache file action wait for file to open it.
                action_cache = (name,) + args
                self.action_cache_dict[file_key].append(action_cache)
                logger.info("Cache action {}, wait for file {} to open it before executing.".format(name, filepath))

        setattr(self, "_{}".format(name), _do)

        def _do_wrap(*args):
            # We need post function event_loop, otherwise long-time calculation will block Emacs.
            self.event_queue.put({
                "name": "action_func",
                "content": ("_{}".format(name), list(map(epc_arg_transformer, args)))
            })

        setattr(self, name, _do_wrap)

    def handle_server_message(self, filepath, request_type, request_id, response_result):
        file_key = path_as_key(filepath)
        if file_key in self.file_action_dict:
            self.file_action_dict[file_key].handle_server_response_message(request_id, request_type, response_result)
        else:
            # Please report bug if you got this message.
            logger.error("IMPOSSIBLE HERE: handle_server_message {} {} {} {}".format(filepath, request_type, request_id, response_result))

    def handle_server_file_opened(self, filepath):
        file_key = path_as_key(filepath)
        self.file_opened[filepath] = True
        if file_key in self.action_cache_dict:
            for action_name, *action_args in self.action_cache_dict[file_key]:
                if file_key in self.file_action_dict:
                    # Execute file action after file opened.
                    self.file_action_dict[file_key].call(action_name, *action_args)
                    logger.info("Execute action {} for file {}".format(action_name, filepath))
                else:
                    # Please report bug if you got this message.
                    logger.error("IMPOSSIBLE HERE: handle_server_file_opened '{}' {} {}".format(filepath, action_name,
                                                                                                self.file_action_dict))

            # We need clear action_cache_dict last.
            del self.action_cache_dict[file_key]

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

    def start_test(self):
        # Called from lsp-bridge-test.el to start test.
        from test.test import start_test
        start_test()


if __name__ == "__main__":
    LspBridge(sys.argv[1:])
