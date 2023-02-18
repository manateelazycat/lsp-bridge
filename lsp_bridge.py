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
import traceback
import json
from pathlib import Path

from epc.server import ThreadingEPCServer

from core.fileaction import (create_file_action_with_single_server,
                             create_file_action_with_multi_servers,
                             FILE_ACTION_DICT, LSP_SERVER_DICT)
from core.lspserver import LspServer
from core.search_file_words import SearchFileWords
from core.search_sdcv_words import SearchSdcvWords
from core.search_list import SearchList
from core.search_tailwindcss_keywords import SearchTailwindKeywords
from core.search_paths import SearchPaths
from core.tabnine import TabNine
from core.utils import *
from core.handler import *

class LspBridge:
    def __init__(self, args):
        # Build EPC interfaces.
        handler_subclasses = list(map(lambda cls: cls.name, Handler.__subclasses__()))
        for name in ["change_file", "update_file",  "save_file",
                     "change_cursor",
                     "ignore_diagnostic", "list_diagnostics",
                     "try_code_action",
                     "workspace_symbol"] + handler_subclasses:
            self.build_file_action_function(name)
            
        for name in ["open_file", "close_file"]:
            self.build_message_function(name)
            
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
        
        # Init tabnine.
        self.tabnine = TabNine()        

        # Init search file words.
        self.search_file_words = SearchFileWords()
        for name in ["index_files", "change_file", "close_file", "rebuild_cache", "search"]:
            self.build_prefix_function("search_file_words", "search_file_words", name)
            
        # Init search sdcv words.
        self.search_sdcv_words = SearchSdcvWords()
        for name in ["search"]:
            self.build_prefix_function("search_sdcv_words", "search_sdcv_words", name)
            
        # Init search list.
        self.search_list = SearchList()
        for name in ["search", "update"]:
            self.build_prefix_function("search_list", "search_list", name)
            
        # Init search tailwind keywords
        self.search_tailwind_keywords = SearchTailwindKeywords()
        for name in ["search"]:
            self.build_prefix_function("search_tailwind_keywords", "search_tailwind_keywords", name)

        # Init search paths.
        self.search_paths = SearchPaths()
        for name in ["search"]:
            self.build_prefix_function("search_paths", "search_paths", name)
            
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
        try:
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
        except:
            logger.error(traceback.format_exc())

    def message_dispatcher(self):
        try:
            while True:
                message = self.message_queue.get(True)
                if message["name"] == "server_process_exit":
                    self.handle_server_process_exit(message["content"])
                else:
                    logger.error("Unhandled lsp-bridge message: %s" % message)
            
                self.message_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def rename_file(self, old_filepath, new_filepath):
        if is_in_path_dict(FILE_ACTION_DICT, old_filepath):
            get_from_path_dict(FILE_ACTION_DICT, old_filepath).rename_file(old_filepath, new_filepath)
        
    def fetch_completion_item_info(self, filepath, item_key, server_name):
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            get_from_path_dict(FILE_ACTION_DICT, filepath).completion_item_resolve(item_key, server_name)
    
    def _open_file(self, filepath):
        project_path = get_project_path(filepath)
        multi_lang_server = get_emacs_func_result("get-multi-lang-server", project_path, filepath)
        
        if multi_lang_server:
            # Try to load multi language server when get-multi-lang-server return match one.
            multi_lang_server_dir = Path(__file__).resolve().parent / "multiserver"
            multi_lang_server_path = multi_lang_server_dir / "{}.json".format(multi_lang_server)
            
            user_multi_lang_server_dir = Path(str(get_emacs_var("lsp-bridge-user-multiserver-dir"))).expanduser()
            user_multi_lang_server_path = user_multi_lang_server_dir / "{}.json".format(multi_lang_server)
            if user_multi_lang_server_path.exists():
                multi_lang_server_path = user_multi_lang_server_path

            with open(multi_lang_server_path, encoding="utf-8", errors="ignore") as f:
                multi_lang_server_info = json.load(f)
                servers = self.pick_multi_server_names(multi_lang_server_info)
                
                # Load multi language server only when all language server commands exist.
                if self.check_multi_server_command(servers, filepath):
                    multi_servers = {}

                    for server_name in servers:
                        server_path = get_lang_server_path(server_name)

                        with open(server_path, encoding="utf-8", errors="ignore") as server_path_file:
                            lang_server_info = read_lang_server_info(server_path_file)
                            lsp_server = self.create_lsp_server(
                                filepath,
                                project_path,
                                lang_server_info,
                                server_name in multi_lang_server_info.get("diagnostics", []))
                            if lsp_server:
                                multi_servers[lang_server_info["name"]] = lsp_server
                            else:
                                return False

                    self.enjoy_hacking(servers, project_path)

                    create_file_action_with_multi_servers(filepath, multi_lang_server_info, multi_servers)
                else:
                    # Try load single language server if multi language server load failed.
                    single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)

                    if single_lang_server:
                        self.load_single_lang_server(project_path, filepath)
                    else:
                        self.turn_off(
                            filepath,
                            "ERROR: can't find all command of multi-server for {}, haven't found match single-server, disable lsp-bridge-mode.".format(filepath))
        else:
            # Try to load single language server.
            self.load_single_lang_server(project_path, filepath)

        return True

    def enjoy_hacking(self, servers, project_path):
        # Notify user server is ready.
        message_emacs("Start LSP server ({}) for {} with '{}' mode, enjoy hacking!".format(
            ", ".join(servers),
            project_path,
            "project" if os.path.isdir(project_path) else "single-file"
        ))

    def load_single_lang_server(self, project_path, filepath):
        single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)

        if not single_lang_server:
            self.turn_off(filepath, "ERROR: can't find the corresponding server for {}, disable lsp-bridge-mode.".format(filepath))

            return False

        lang_server_info = load_single_server_info(single_lang_server)

        if ((not os.path.isdir(project_path)) and
            "support-single-file" in lang_server_info and
            lang_server_info["support-single-file"] is False):
            self.turn_off(
                filepath,
                "ERROR: {} not support single-file, put this file in a git repository to enable lsp-bridge-mode.".format(single_lang_server))

            return False

        lsp_server = self.create_lsp_server(filepath, project_path, lang_server_info)

        if lsp_server:
            self.enjoy_hacking([lang_server_info["name"]], project_path)

            create_file_action_with_single_server(filepath, lang_server_info, lsp_server)
        else:
            return False
    
    def turn_off(self, filepath, message):
        message_emacs(message)
        eval_in_emacs("lsp-bridge--turn-off", filepath)

    def check_lang_server_command(self, lang_server_info, filepath, turn_off_on_error=True):
        if len(lang_server_info["command"]) > 0:
            server_command = lang_server_info["command"][0]
            server_command_path = shutil.which(server_command)

            if server_command_path:
                # We always replace LSP server command with absolute path of 'which' command.
                lang_server_info["command"][0] = server_command_path

                return True
            else:
                error_message = "Error: can't find command '{}' to start LSP server {} ({})".format(
                    server_command, lang_server_info["name"], filepath)

                if turn_off_on_error:
                    self.turn_off(filepath, error_message + ", disable lsp-bridge-mode.")
                else:
                    message_emacs(error_message)

                return False
        else:
            error_message = "Error: {}'s command argument is empty".format(filepath)

            if turn_off_on_error:
                self.turn_off(filepath, error_message + ", disable lsp-bridge-mode.")
            else:
                message_emacs(error_message)

            return False

    def check_multi_server_command(self, server_names, filepath):
        for server_name in server_names:
            server_path = get_lang_server_path(server_name)

            with open(server_path, encoding="utf-8", errors="ignore") as server_path_file:
                lang_server_info = read_lang_server_info(server_path_file)

                if not self.check_lang_server_command(lang_server_info, filepath, False):
                    return False

        return True

    def create_lsp_server(self, filepath, project_path, lang_server_info, enable_diagnostics=True):
        if not self.check_lang_server_command(lang_server_info, filepath):
            return False

        lsp_server_name = "{}#{}".format(path_as_key(project_path), lang_server_info["name"])
                                
        if lsp_server_name not in LSP_SERVER_DICT:
            LSP_SERVER_DICT[lsp_server_name] = LspServer(
                message_queue=self.message_queue,
                project_path=project_path,
                server_info=lang_server_info,
                server_name=lsp_server_name,
                enable_diagnostics=enable_diagnostics)
            
        return LSP_SERVER_DICT[lsp_server_name]
    
    def pick_multi_server_names(self, multi_lang_server_info):
        servers = []
        for info in multi_lang_server_info:
            info_value = multi_lang_server_info[info]
            if type(info_value) == str:
                servers.append(info_value)
            else:
                servers += info_value
        
        return list(dict.fromkeys(servers))

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
        
    def build_prefix_function(self, obj_name, prefix, name):
        def _do(*args, **kwargs):
            getattr(getattr(self, obj_name), name)(*args, **kwargs)

        setattr(self, "{}_{}".format(prefix, name), _do)
        
    def build_message_function(self, name):
        def _do(filepath):
            self.event_queue.put({
                "name": name,
                "content": filepath
            })
            
        setattr(self, name, _do)
        
    def tabnine_complete(self, before, after, filename, region_includes_beginning, region_includes_end, max_num_results):
        self.tabnine.complete(before, after, filename, region_includes_beginning, region_includes_end, max_num_results)
            
    def handle_server_process_exit(self, server_name):
        if server_name in LSP_SERVER_DICT:
            log_time("Exit server {}".format(server_name))
            del LSP_SERVER_DICT[server_name]
            
    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

    def start_test(self):
        # Called from lsp-bridge-test.el to start test.
        from test.test import start_test
        start_test(self)
        
    def profile_dump(self):
        try:
            global profiler
            profiler.dump_stats(os.path.expanduser("~/lsp-bridge.prof"))
            message_emacs("Output profile data to ~/lsp-bridge.prof, please use snakeviz open it.")
        except:
            message_emacs("Set option 'lsp-bridge-enable-profile' to 't' and call lsp-bridge-restart-process, then call lsp-bridge-profile-dump again.")

def read_lang_server_info(lang_server_path):
    return server_info_replace_template(json.load(lang_server_path))

def load_single_server_info(lang_server):
    lang_server_info_path = ""
    if os.path.exists(lang_server) and os.path.dirname(lang_server) != "":
        # If lang_server is real file path, we load the LSP server configuration from the user specified file.
        lang_server_info_path = lang_server
    else:
        # Otherwise, we load LSP server configuration from file lsp-bridge/langserver/lang_server.json.
        lang_server_info_path = get_lang_server_path(lang_server)
        
    with open(lang_server_info_path, encoding="utf-8", errors="ignore") as f:
        return read_lang_server_info(f)

def replace_template(arg):
    if "%USER_EMACS_DIRECTORY%" in arg:
        user_emacs_dir = get_emacs_func_result("get-user-emacs-directory").replace("/", "\\")
        return arg.replace("%USER_EMACS_DIRECTORY%", user_emacs_dir)
    elif "$HOME" in arg:
            return os.path.expandvars(arg)
    elif "%FILEHASH%" in arg:
        # pyright use `--cancellationReceive` option enable "background analyze" to improve completion performance.
        return arg.replace("%FILEHASH%", os.urandom(21).hex())
    elif "%USERPROFILE%" in arg:
        return arg.replace("%USERPROFILE%", windows_get_env_value("USERPROFILE"))
    else:
        return arg

def server_info_replace_template(lang_server_info):
    # Replace template in command options.
    command_args = lang_server_info["command"]
    for i, arg in enumerate(command_args):
        command_args[i] = replace_template(arg)
    lang_server_info["command"] = command_args

    # Replace template in initializationOptions.
    if "initializationOptions" in lang_server_info:
        initialization_options_args = lang_server_info["initializationOptions"]
        for i, arg in enumerate(initialization_options_args):
            if type(initialization_options_args[arg]) == str:
                initialization_options_args[arg] = replace_template(initialization_options_args[arg])
        lang_server_info["initializationOptions"] = initialization_options_args

    return lang_server_info

def get_lang_server_path(server_name):
    server_dir = Path(__file__).resolve().parent / "langserver"
    server_path_current = server_dir / "{}_{}.json".format(server_name, get_os_name())
    server_path_default = server_dir / "{}.json".format(server_name)

    user_server_dir = Path(str(get_emacs_var("lsp-bridge-user-langserver-dir"))).expanduser()
    user_server_path_current = user_server_dir / "{}_{}.json".format(server_name, get_os_name())
    user_server_path_default = user_server_dir / "{}.json".format(server_name)

    if user_server_path_current.exists():
        server_path_current = user_server_path_current
    elif user_server_path_default.exists():
        server_path_current = user_server_path_default

    return server_path_current if server_path_current.exists() else server_path_default
    
if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("LspBridge(sys.argv[1:])")
    else:
        LspBridge(sys.argv[1:])
    
