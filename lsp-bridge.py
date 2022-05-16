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

from core.fileaction import FileAction
from core.lspserver import LspServer
from core.utils import *
from epc.server import ThreadingEPCServer
import os
import platform
import queue
import signal
import sys
import threading

class LspBridge(object):
    def __init__(self, args):
        object.__init__(self)

        # Object cache to exchange information between Emacs and LSP server.
        self.file_action_dict = {}  # use for contain file action
        self.lsp_server_dict = {}   # use for contain lsp server
        self.action_cache_dict = {} # use for contain file action cache
        self.workspace_server_dict = {} # use for save the mapping between workspace and LSP server name

        # Build EPC interfaces.
        for name in ["change_file", "find_define", "find_implementation", "find_references", 
                     "prepare_rename", "rename", "change_cursor", "save_file", "hover", "signature_help"]:
            self.build_file_action_function(name)

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

        self.server.register_instance(self) # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Init emacs option.
        (enable_lsp_server_log, ) = get_emacs_vars(["lsp-bridge-enable-log"])
        if enable_lsp_server_log:
            logger.setLevel(logging.DEBUG)
        self.workspace_dir = get_emacs_var("lsp-bridge-workspace-dir")

        # All Emacs request running in postgui_thread.
        self.postgui_queue = queue.Queue()
        self.postgui_thread = threading.Thread(target=self.postgui_dispatcher)
        self.postgui_thread.start()

        # All LSP server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        # Pass epc port and webengine codec information to Emacs when first start LspBridge.
        eval_in_emacs('lsp-bridge--first-start', [self.server.server_address[1]])

        # postgui_thread nevert exit, simulation event loop.
        self.postgui_thread.join()

    def postgui_dispatcher(self):
        while True:
            message = self.postgui_queue.get(True)

            if message["name"] == "open_file":
                self._open_file(message["content"])
            elif message["name"] == "close_file":
                self._close_file(message["content"])
            elif message["name"] == "action_func":
                (func_name, func_args) = message["content"]
                getattr(self, func_name)(*func_args)

            self.postgui_queue.task_done()

    def message_dispatcher(self):
        while True:
            message = self.message_queue.get(True)
            if message["name"] == "server_file_opened":
                self.handle_server_file_opened(message["content"])
            elif message["name"] == "server_process_exit":
                self.handle_server_process_exit(message["content"])
            elif message["name"] == "server_response_message":
                self.handle_server_message(*message["content"])
            elif message["name"] == "init_workspace":
                self.workspace_server_dict[message["content"]["workspace"]] = message["content"]["lsp_server_name"]


            self.message_queue.task_done()

    def open_file(self, filepath):
        # We need post function postgui_thread, otherwise long-time calculation will block Emacs.
        self.postgui_queue.put({
            "name": "open_file",
            "content": filepath
        })

    def _open_file(self, filepath):
        # Create file action.
        filekey = path_as_key(filepath)
        project_path = get_project_path(filepath)
        lang_server = get_emacs_func_result("get-lang-server", [project_path, filepath])
        lsp_server_name = None
        if filepath.startswith(self.workspace_dir):
            workspace = "{}#{}".format(self.workspace_dir, lang_server)
            lsp_server_name = self.workspace_server_dict[workspace]
        if filekey not in self.file_action_dict:
            action = FileAction(filepath, project_path, lang_server)
            if lsp_server_name is None:
                lsp_server_name = action.get_lsp_server_name()
            self.file_action_dict[filekey] = action

        # Create LSP server.
        file_action = self.file_action_dict[filekey]
        if project_path in self.workspace_server_dict:
            lsp_server_name = self.workspace_server_dict[project_path]
        if lsp_server_name not in self.lsp_server_dict:
            # lsp server will send initialize and didOpen when open first file in project.
            server = LspServer(self.message_queue, file_action)
            self.lsp_server_dict[lsp_server_name] = server
        else:
            # Send didOpen notification to LSP server.
            self.lsp_server_dict[lsp_server_name].send_did_open_notification(file_action.filepath, file_action.filepath)

        # Add lsp server in file action for send message to lsp server.
        file_action.lsp_server = self.lsp_server_dict[lsp_server_name]

    def close_file(self, filepath):
        # We need post function postgui_thread, otherwise long-time calculation will block Emacs.
        self.postgui_queue.put({
            "name": "close_file",
            "content": filepath
        })

    def _close_file(self, filepath):
        filekey = path_as_key(filepath)
        if filekey in self.file_action_dict:
            action = self.file_action_dict[filekey]

            lsp_server_name = action.get_lsp_server_name()
            if lsp_server_name in self.lsp_server_dict:
                lsp_server = self.lsp_server_dict[lsp_server_name]
                lsp_server.close_file(filepath)

            del self.file_action_dict[filekey]

    def build_file_action_function(self, name):
        def _do(*args):
            filepath = args[0]
            filekey = path_as_key(filepath)
            if filekey in self.file_action_dict:
                action = self.file_action_dict[filekey]
                getattr(action, name)(*args[1:])
            else:
                # Cache file action wait for file to open it.
                action_cache = (name, ) + args[1:]
                if filekey in self.action_cache_dict:
                    self.action_cache_dict[filekey].append(action_cache)
                else:
                    self.action_cache_dict[filekey] = [action_cache]
                    
                self.open_file(filepath)
                logger.info("Cache action {}, wait for file {} to open it before executing.".format(name, filepath))

        setattr(self, "_{}".format(name), _do)

        def _do_wrap(*args):
            # We need post function postgui_thread, otherwise long-time calculation will block Emacs.
            self.postgui_queue.put({
                "name": "action_func",
                "content": ("_{}".format(name), list(map(epc_arg_transformer, args)))
            })

        setattr(self, name, _do_wrap)

    def handle_server_message(self, filepath, request_type, request_id, response_result):
        filekey = path_as_key(filepath)
        if filekey in self.file_action_dict:
            self.file_action_dict[filekey].handle_server_response_message(request_id, request_type, response_result)
        else:
            # Please report bug if you got this message.
            logger.error("IMPOSSIBLE HERE: handle_server_message %s %s", filepath, request_type, request_id, response_result)

    def handle_server_process_exit(self, server_name):
        if server_name in self.lsp_server_dict:
            logger.info("Exit server: {}".format(server_name))
            del self.lsp_server_dict[server_name]

    def handle_server_file_opened(self, filepath):
        filekey = path_as_key(filepath)
        if filekey in self.action_cache_dict:
            for action_cache in self.action_cache_dict[filekey]:
                action_name = action_cache[0]
                action_args = action_cache[1:]
                
                if filekey in self.file_action_dict:
                    # Execute file action after file opened.
                    getattr(self.file_action_dict[filekey], action_name)(*action_args)
                    logger.info("Execute action {} for file {}".format(action_name, filepath))
                else:
                    # Please report bug if you got this message.
                    logger.error("IMPOSSIBLE HERE: handle_server_file_opened '{}' {} {}".format(filepath, action_name, self.file_action_dict))
                    
            # We need clear action_cache_dict last.
            del self.action_cache_dict[filekey]

    def cleanup(self):
        '''Do some cleanup before exit python process.'''
        close_epc_client()

    def start_test(self):
        from test.test import start_test
        start_test()


if __name__ == "__main__":
    LspBridge(sys.argv[1:])
