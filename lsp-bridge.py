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

from PyQt6.QtWidgets import QApplication
from epc.server import ThreadingEPCServer
import os
import platform
import signal
import sys
import threading
import queue

from core.fileaction import FileAction
from core.lspserver import LspServer
from core.utils import (init_epc_client, close_epc_client, eval_in_emacs, get_emacs_vars)

class LspBridge(object):
    def __init__(self, args):
        object.__init__(self)
        
        self.file_action_dict = {}
        self.lsp_server_dict = {}
        self.action_cache_dict = {}

        for name in ["change_file", "find_define", "find_references", "prepare_rename", "rename", "change_cursor"]:
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
        
        self.postgui_queue = queue.Queue()
        self.postgui_thread = threading.Thread(target=self.postgui_dispatcher)
        self.postgui_thread.start()
        
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()
        
        # Pass epc port and webengine codec information to Emacs when first start LspBridge.
        eval_in_emacs('lsp-bridge--first-start', [self.server.server_address[1]])
        
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
            
            self.message_queue.task_done()

    def open_file(self, filepath):
        self.postgui_queue.put({
            "name": "open_file",
            "content": filepath
        })
        
    def _open_file(self, filepath):
        # Build file action.
        if filepath not in self.file_action_dict:
            action = FileAction(filepath)
            self.file_action_dict[filepath] = action
            
        # Build LSP server.
        file_action = self.file_action_dict[filepath]
        lsp_server_name = file_action.get_lsp_server_name()
        if lsp_server_name not in self.lsp_server_dict:
            server = LspServer(self.message_queue, file_action) # lsp server will initialize and didOpen for first file
            self.lsp_server_dict[lsp_server_name] = server
        else:
            # Did open file if lsp server has exists, usually other file in same project has opened. 
            self.lsp_server_dict[lsp_server_name].send_did_open_notification(file_action.filepath)
        
        # Add lsp server in file action for send message to lsp server.
        file_action.lsp_server = self.lsp_server_dict[lsp_server_name]
            
    def close_file(self, filepath):
        self.postgui_queue.put({
            "name": "close_file",
            "content": filepath
        })
        
    def _close_file(self, filepath):
        if filepath in self.file_action_dict:
            action = FileAction(filepath)
            
            lsp_server_name = action.get_lsp_server_name()
            if lsp_server_name in self.lsp_server_dict:
                lsp_server = self.lsp_server_dict[lsp_server_name]
                lsp_server.close_file(filepath)
                
            del self.file_action_dict[filepath]
            
    def build_file_action_function(self, name):
        def _do(*args):
            filepath = args[0]
            if filepath in self.file_action_dict:
                action = self.file_action_dict[filepath]
                getattr(action, name)(*args[1:])
            else:
                self.action_cache_dict[filepath] = (name, ) + args[1:]
                self.open_file(filepath)
                print("Cache action {}, wait for file {} to open it before executing.".format(name, filepath))
                
        setattr(self, "_{}".format(name), _do)
        
        def _do_wrap(*args):
            self.postgui_queue.put({
                "name": "action_func",
                "content": ("_{}".format(name), args)
            })
            
        setattr(self, name, _do_wrap)
        
    def handle_server_message(self, filepath, request_type, request_id, response_result):
        if filepath in self.file_action_dict:
            self.file_action_dict[filepath].handle_server_response_message(request_id, request_type, response_result)
        else:
            # Please report bug if you got this message.
            print("IMPOSSIBLE HERE: handle_server_message ", filepath, request_type, request_id, response_result)
            
            
    def handle_server_process_exit(self, server_name):
        if server_name in self.lsp_server_dict:
            print("Exit server: ", server_name)
            del self.lsp_server_dict[server_name]
            
    def handle_server_file_opened(self, filepath):
        if filepath in self.action_cache_dict:
            cache = self.action_cache_dict[filepath]
            action_name = cache[0]
            action_args = cache[1:]
            
            if filepath in self.file_action_dict:
                getattr(self.file_action_dict[filepath], action_name)(*action_args)
                print("Execute action {} for file {}".format(action_name, filepath))
            else:
                # Please report bug if you got this message.
                print("IMPOSSIBLE HERE: handle_server_file_opened '{}' {} {}".format(filepath, action_name, self.file_action_dict))

    def cleanup(self):
        '''Do some cleanup before exit python process.'''
        close_epc_client()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    LspBridge(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec())
