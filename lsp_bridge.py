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
import queue
import shutil
import threading
from pathlib import Path
from typing import Dict

from epc.server import ThreadingEPCServer

from core.fileaction import FileAction, create_file_action, FILE_ACTION_DICT, LSP_SERVER_DICT
from core.lspserver import LspServer
from core.search_file_words import SearchFileWords
from core.utils import *
from core.handler import *

class LspBridge:
    def __init__(self, args):

        # Object cache to exchange information between Emacs and LSP server.
        self.search_file_words = SearchFileWords()

        # Build EPC interfaces.
        for name in ["change_file", "change_cursor", "save_file", "ignore_diagnostic", "list_diagnostics"]:
            self.build_file_action_function(name)
            
        for name in ["change_file", "close_file", "rebuild_cache", "search"]:
            self.build_search_words_function(name)
            
        for name in ["open_file", "close_file"]:
            self.build_message_function(name)
            
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
        # self.server.logger = logger

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

        # Pass epc port and webengine codec information to Emacs when first start lsp-bridge.
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
            if message["name"] == "server_process_exit":
                self.handle_server_process_exit(message["content"])
            else:
                logger.error("Unhandled lsp-bridge message: %s" % message)

            self.message_queue.task_done()

    def rename_file(self, old_filepath, new_filepath):
        if is_in_path_dict(FILE_ACTION_DICT, old_filepath):
            get_from_path_dict(FILE_ACTION_DICT, old_filepath).rename_file(old_filepath, new_filepath)
        
    def completion_hide(self, filepath):
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            get_from_path_dict(FILE_ACTION_DICT, filepath).last_completion_candidates = []
            
    def pull_diagnostics(self, filepath):
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            eval_in_emacs("lsp-bridge-diagnostics-render", filepath, get_from_path_dict(FILE_ACTION_DICT, filepath).diagnostics)
            
    def fetch_completion_item_info(self, filepath, item_key):
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            get_from_path_dict(FILE_ACTION_DICT, filepath).completion_item_resolve(item_key)
    
    def _open_file(self, filepath):
        project_path = get_project_path(filepath)
        lang_server = get_emacs_func_result("get-lang-server", project_path, filepath)

        if (lang_server == "clojure-lsp") and (not os.path.isdir(project_path)):
            message_emacs("ERROR: can't determine the project root for {}, initialize a Git repository for the project before you open this file.".format(filepath))
            eval_in_emacs("lsp-bridge-turn-off", filepath)

            return False

        if not lang_server:
            message_emacs("ERROR: can't find the corresponding server for {}, disable lsp-bridge-mode.".format(filepath))
            eval_in_emacs("lsp-bridge-turn-off", filepath)

            return False

        lang_server_info = load_lang_server_info(lang_server)
        
        if len(lang_server_info["command"]) > 0:
            server_command = lang_server_info["command"][0]
            server_command_path = shutil.which(server_command)
            if server_command_path:
                # We always replace LSP server command with absolute path of 'which' command.
                lang_server_info["command"][0] = server_command_path
            else:
                message_emacs("Error: can't find LSP server '{}' for {}, disable lsp-bridge-mode.".format(server_command, filepath))
                eval_in_emacs("lsp-bridge-turn-off", filepath)

                return False
        else:
            message_emacs("Error: {}'s command argument is empty, disable lsp-bridge-mode.".format(filepath))
            eval_in_emacs("lsp-bridge-turn-off", filepath)

            return False
        
        lsp_server_name = "{}#{}".format(path_as_key(project_path), lang_server_info["name"])

        if lsp_server_name not in LSP_SERVER_DICT:
            LSP_SERVER_DICT[lsp_server_name] = LspServer(
                message_queue=self.message_queue,
                project_path=project_path,
                server_info=lang_server_info,
                server_name=lsp_server_name
            )

        lsp_server = LSP_SERVER_DICT[lsp_server_name]

        create_file_action(filepath, lang_server_info, lsp_server)
        
        return True

    def _close_file(self, filepath):
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            get_from_path_dict(FILE_ACTION_DICT, filepath).exit()
            
    def build_file_action_function(self, name):
        def _do(filepath, *args):
            open_file_success = True

            if not is_in_path_dict(FILE_ACTION_DICT, filepath):
                open_file_success = self._open_file(filepath)  # _do is called inside event_loop, so we can block here.

            if open_file_success:
                action = get_from_path_dict(FILE_ACTION_DICT, filepath)
                action.call(name, *args)

        setattr(self, "_{}".format(name), _do)

        def _do_wrap(*args):
            # We need post function event_loop, otherwise long-time calculation will block Emacs.
            self.event_queue.put({
                "name": "action_func",
                "content": ("_{}".format(name), list(map(epc_arg_transformer, args)))
            })

        setattr(self, name, _do_wrap)
        
    def build_search_words_function(self, name):
        def _do(*args, **kwargs):
            getattr(self.search_file_words, name)(*args, **kwargs)

        setattr(self, "search_words_{}".format(name), _do)

    def build_message_function(self, name):
        def _do(filepath):
            self.event_queue.put({
                "name": name,
                "content": filepath
            })
            
        setattr(self, name, _do)
            
    def handle_server_process_exit(self, server_name):
        if server_name in LSP_SERVER_DICT:
            logger.info("Exit server: {}".format(server_name))
            del LSP_SERVER_DICT[server_name]
            
    def search_words_index_files(self, filepaths):
        for filepath in filepaths:
            self.search_file_words.change_file(filepath)
        
    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

    def start_test(self):
        # Called from lsp-bridge-test.el to start test.
        from test.test import start_test
        start_test(self)


def load_lang_server_info(lang_server):
    lang_server_info_path = ""
    if os.path.exists(lang_server) and os.path.dirname(lang_server) != "":
        # If lang_server is real file path, we load the LSP server configuration from the user specified file.
        lang_server_info_path = lang_server
    else:
        # Otherwise, we load LSP server configuration from file lsp-bridge/langserver/lang_server.json.
        lang_server_dir = Path(__file__).resolve().parent / "langserver"
        lang_server_file_path_current = lang_server_dir / "{}_{}.json".format(lang_server, get_os_name())
        lang_server_file_path_default = lang_server_dir / "{}.json".format(lang_server)

        lang_server_info_path = lang_server_file_path_current if lang_server_file_path_current.exists() else lang_server_file_path_default

    with open(lang_server_info_path, encoding="utf-8") as f:
        import json
        return json.load(f)
    
if __name__ == "__main__":
    LspBridge(sys.argv[1:])
