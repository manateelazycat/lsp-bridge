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
import socket
from functools import wraps
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
from core.codeium import Codeium
from core.utils import *
from core.handler import *
from core.remote_file import RemoteFileClient, RemoteFileServer, save_ip

def threaded(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper

REMOTE_FILE_SYNC_CHANNEL = 9999
REMOTE_FILE_COMMAND_CHANNEL = 9998
REMOTE_FILE_ELISP_CHANNEL = 9997

class LspBridge:
    def __init__(self, args):
        # Check running environment.
        self.running_in_server = len(args) == 0
        if self.running_in_server:
            set_running_in_server()

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        self.server.allow_reuse_address = True
        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Init variables.
        self.thread_queue = []
        self.client_dict = {}
        self.lsp_client_dict = {}
        self.host_names = {}

        # Init event loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        if self.running_in_server:
            # Start remote file server if lsp-bridge running in server.
            self.init_remote_file_server()
        else:
            # Init EPC client and search backends if lsp-bridge running in local.
            init_epc_client(int(args[0]))
            self.init_search_backends()
            eval_in_emacs('lsp-bridge--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def init_search_backends(self):
        # Init tabnine.
        self.tabnine = TabNine()

        # Init codeium
        self.codeium = Codeium()

        # Init search backends.
        self.search_file_words = SearchFileWords()
        self.search_sdcv_words = SearchSdcvWords()
        self.search_list = SearchList()
        self.search_tailwind_keywords = SearchTailwindKeywords()
        self.search_paths = SearchPaths()

        # Build EPC interfaces.
        handler_subclasses = list(map(lambda cls: cls.name, Handler.__subclasses__()))
        for name in ["change_file", "update_file",  "save_file",
                     "try_completion", "change_cursor",
                     "ignore_diagnostic", "list_diagnostics",
                     "try_code_action",
                     "workspace_symbol"] + handler_subclasses:
            self.build_file_action_function(name)

        search_backend_export_functions = {
            "search_file_words": ["index_files", "change_buffer", "load_file", "close_file", "search"],
            "search_sdcv_words": ["search"],
            "search_list": ["search", "update"],
            "search_tailwind_keywords": ["search"],
            "search_paths": ["search"]
        }
        for search_backend, export_functions in search_backend_export_functions.items():
            for name in export_functions:
                self.build_prefix_function(search_backend, search_backend, name)

        # Set log level.
        [enable_lsp_server_log] = get_emacs_vars(["lsp-bridge-enable-log"])
        if enable_lsp_server_log:
            logger.setLevel(logging.DEBUG)

        # All LSP server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        # Build loop to open remote file.
        self.remote_file_sender_queue = queue.Queue()
        self.remote_file_sender_thread = threading.Thread(target=self.send_message_dispatcher,
                                                          args=(self.remote_file_sender_queue, REMOTE_FILE_SYNC_CHANNEL))
        self.remote_file_sender_thread.start()

        # Build loop to call remote Python command.
        self.remote_file_command_sender_queue = queue.Queue()
        self.remote_file_command_sender_thread = threading.Thread(target=self.send_message_dispatcher,
                                                         args=(self.remote_file_command_sender_queue, REMOTE_FILE_COMMAND_CHANNEL))
        self.remote_file_command_sender_thread.start()

        # Build loop to send remote file to local Emacs.
        self.remote_file_receiver_queue = queue.Queue()
        self.remote_file_receiver_thread = threading.Thread(target=self.receive_message_dispatcher,
                                                            args=(self.remote_file_receiver_queue, self.handle_remote_file_message))
        self.remote_file_receiver_thread.start()

        # Build loop to send elisp command from remote server to local Emacs.
        self.remote_file_command_receiver_queue = queue.Queue()
        self.remote_file_command_receiver_thread = threading.Thread(target=self.receive_message_dispatcher,
                                                           args=(self.remote_file_command_receiver_queue, self.handle_lsp_message))
        self.remote_file_command_receiver_thread.start()

    def send_message_dispatcher(self, queue, port):
        try:
            while True:
                data = queue.get(True)

                client = self.get_socket_client(data["host"], port)
                client.send_message(data["message"])

                queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def receive_message_dispatcher(self, queue, handle_remote_file_message):
        try:
            while True:
                message = queue.get(True)
                handle_remote_file_message(message)
                queue.task_done()
        except:
            logger.error(traceback.format_exc())

    @threaded
    def open_remote_file(self, path, jump_define_pos):
        if is_valid_ip_path(path):
            import paramiko
            from urllib.parse import urlparse

            parsed_url = urlparse('ssh://' + path)

            server_host = parsed_url.hostname
            server_path = parsed_url.path
            ssh_port = parsed_url.port
            server_username = parsed_url.username

            if not server_username:
                if server_host in self.host_names:
                    server_username = self.host_names[server_host]["username"]
                else:
                    server_username = "root"

            if not ssh_port:
                if server_host in self.host_names:
                    ssh_port = self.host_names[server_host]["ssh_port"]
                else:
                    ssh_port = 22

            if server_username and ssh_port:
                self.host_names[server_host] = {"username": server_username, "ssh_port": ssh_port}

            try:
                client_id = f"{server_host}:{REMOTE_FILE_ELISP_CHANNEL}"
                if client_id not in self.client_dict:
                    client = self.get_socket_client(server_host, REMOTE_FILE_ELISP_CHANNEL)
                    client.send_message("Connect")

                message_emacs(f"Open {server_username}@{server_host}#{ssh_port}:{server_path}...")

                self.send_remote_file_message(server_host, {
                    "command": "open_file",
                    "server": server_host,
                    "path": server_path,
                    "jump_define_pos": epc_arg_transformer(jump_define_pos)
                })

                save_ip(f"{server_username}@{server_host}:{ssh_port}")
            except paramiko.ssh_exception.ChannelException:
                message_emacs(f"Connect {server_username}@{server_host}:{ssh_port} failed, please make sure `lsp_bridge.py` has start at server.")

        else:
            message_emacs("Please input valid path match rule: 'ip:/path/file'.")

    @threaded
    def save_remote_file(self, remote_file_host, remote_file_path):
        self.send_remote_file_message(remote_file_host, {
            "command": "save_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

    @threaded
    def close_remote_file(self, remote_file_host, remote_file_path):
        self.send_remote_file_message(remote_file_host, {
            "command": "close_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

    @threaded
    def handle_remote_file_message(self, message):
        data = parse_json_content(message)
        command = data["command"]

        if command == "open_file":
            if "error" in data:
                message_emacs(data["error"])
            else:
                server = data["server"]
                path = data["path"]
                eval_in_emacs("lsp-bridge-open-remote-file--response", data["server"], path, string_to_base64(data["content"]), data["jump_define_pos"])
                message_emacs(f"Open file {server}:{path}")

    @threaded
    def handle_lsp_message(self, message):
        data = parse_json_content(message)
        if data["command"] == "eval-in-emacs":
            # Execute emacs command from remote server.
            eval_sexp_in_emacs(data["sexp"])

    @threaded
    def lsp_request(self, remote_file_host, remote_file_path, method, args):
        if method == "change_file":
            self.send_remote_file_message(remote_file_host, {
                "command": "change_file",
                "server": remote_file_host,
                "path": remote_file_path,
                "args": list(map(epc_arg_transformer, args))
            })

        self.send_lsp_bridge_message(remote_file_host, {
            "command": "lsp_request",
            "server": remote_file_host,
            "path": remote_file_path,
            "method": method,
            "args": list(map(epc_arg_transformer, args))
        })

    @threaded
    def func_request(self, remote_file_host, remote_file_path, method, args):
        self.send_lsp_bridge_message(remote_file_host, {
            "command": "func_request",
            "server": remote_file_host,
            "path": remote_file_path,
            "method": method,
            "args": list(map(epc_arg_transformer, args))
        })

    def send_remote_file_message(self, host, message):
        self.remote_file_sender_queue.put({
            "host": host,
            "message": message
        })

    def send_lsp_bridge_message(self, host, message):
        self.remote_file_command_sender_queue.put({
            "host": host,
            "message": message
        })

    @threaded
    def init_remote_file_server(self):
        print("* Running lsp-bridge in remote server, use command 'lsp-bridge-open-remote-file' to open remote file.")

        # Build loop for call local Emacs function from server.
        remote_file_server = RemoteFileServer("0.0.0.0", REMOTE_FILE_SYNC_CHANNEL)
        self.remote_file_server = remote_file_server
        set_remote_file_server(remote_file_server)

        self.remote_file_elisp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.remote_file_elisp.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.remote_file_elisp.bind(("0.0.0.0", REMOTE_FILE_ELISP_CHANNEL))
        self.remote_file_elisp.listen(5)

        self.remote_file_elisp_loop = threading.Thread(target=self.remote_file_elisp_dispatcher)
        self.remote_file_elisp_loop.start()

        # Build loop for call remote command from local Emacs.
        self.remote_request = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.remote_request.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.remote_request.bind(("0.0.0.0", REMOTE_FILE_COMMAND_CHANNEL))
        self.remote_request.listen(5)

        self.remote_request_socket = None

        self.remote_request_loop = threading.Thread(target=self.remote_request_dispatcher)
        self.remote_request_loop.start()

    def get_socket_client(self, server_host, server_port):
        client_id = f"{server_host}:{server_port}"

        if client_id in self.client_dict:
            client = self.client_dict[client_id]
        else:
            client = RemoteFileClient(
                server_host,
                self.host_names[server_host]["username"],
                self.host_names[server_host]["ssh_port"],
                server_port,
                lambda message: self.receive_socket_message(message, server_port))
            client.start()

            self.client_dict[client_id] = client

        return client

    def receive_socket_message(self, message, server_port):
        if server_port == REMOTE_FILE_SYNC_CHANNEL:
            self.remote_file_receiver_queue.put(message)
        elif server_port == REMOTE_FILE_COMMAND_CHANNEL:
            self.remote_file_command_receiver_queue.put(message)
        elif server_port == REMOTE_FILE_ELISP_CHANNEL:
            # Receive elisp RPC call from remote server.
            log_time(f"Receive remote message: {message}")

            data = parse_json_content(message)
            host = data["host"]

            # Read elisp code from local Emacs, and sendback to remote server.
            client = self.get_socket_client(host, REMOTE_FILE_ELISP_CHANNEL)
            if data["command"] == "get_emacs_func_result":
                result = get_emacs_func_result(data["method"], *data["args"])
            elif data["command"] == "get_emacs_vars":
                result = get_emacs_vars(data["args"])

            client.send_message(result)

    def remote_file_elisp_dispatcher(self):
        try:
            while True:
                client_socket, client_address = self.remote_file_elisp.accept()

                # Record first socket client to make remote server call elisp function from local Emacs.
                if get_remote_rpc_socket() is None:
                    log_time(f"Build connect from {client_address[0]}:{client_address[1]}")

                    set_remote_rpc_socket(client_socket, client_address[0])

                    # Drop first "say hello" message from local Emacs.
                    socket_file = client_socket.makefile("r")
                    socket_file.readline().strip()
                    socket_file.close()

                    self.init_search_backends()
        except:
            print(traceback.format_exc())

    def remote_request_dispatcher(self):
        try:
            while True:
                # Record server host when lsp-bridge running in remote server.
                client_socket, client_address = self.remote_request.accept()
                set_lsp_file_host(client_address[0])
                set_remote_eval_socket(client_socket)
                self.remote_request_socket = client_socket

                client_handler = threading.Thread(target=self.handle_remote_request)
                client_handler.start()
        except:
            print(traceback.format_exc())

    def handle_remote_request(self):
        client_file = self.remote_request_socket.makefile('r')
        while True:
            # Receive Python command request from local Emacs.
            message = client_file.readline().strip()
            if not message:
                break

            message = parse_json_content(message)

            if message["command"] == "lsp_request":
                # Call LSP request.
                self.event_queue.put({
                    "name": "action_func",
                    "content": ("_{}".format(message["method"]), [message["path"]] + message["args"])
                })
            elif message["command"] == "func_request":
                # Call lsp-bridge normal function.
                getattr(self, message["method"])(*message["args"])

    def event_dispatcher(self):
        try:
            while True:
                message = self.event_queue.get(True)

                if message["name"] == "close_file":
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

    def open_file(self, filepath):
        project_path = get_project_path(filepath)
        multi_lang_server = get_emacs_func_result("get-multi-lang-server", project_path, filepath)

        if os.path.splitext(filepath)[-1] == '.org':
            single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)
            lang_server_info = load_single_server_info(single_lang_server)
            #TODO support diagnostic
            lsp_server = self.create_lsp_server(filepath, project_path, lang_server_info, enable_diagnostics=False)
            create_file_action_with_single_server(filepath, lang_server_info, lsp_server)
        elif multi_lang_server:
            # Try to load multi language server when get-multi-lang-server return match one.
            multi_lang_server_dir = Path(__file__).resolve().parent / "multiserver"
            multi_lang_server_path = multi_lang_server_dir / "{}.json".format(multi_lang_server)

            user_multi_lang_server_dir = Path(str(get_emacs_vars(["lsp-bridge-user-multiserver-dir"])[0])).expanduser()
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
                        return self.load_single_lang_server(project_path, filepath)
                    else:
                        self.turn_off(
                            filepath,
                            "ERROR: can't find all command of multi-server for {}, haven't found match single-server".format(filepath))

                        return False
        else:
            # Try to load single language server.
            return self.load_single_lang_server(project_path, filepath)

        return True

    def close_file(self, filepath):
        # Add queue, make sure close file after other LSP request.
        self.event_queue.put({
            "name": "close_file",
            "content": filepath
        })

    def _close_file(self, filepath):
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            get_from_path_dict(FILE_ACTION_DICT, filepath).exit()

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
            self.turn_off(filepath, "ERROR: can't find the corresponding server for {}".format(filepath))

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

        return True

    def turn_off(self, filepath, message):
        if os.path.splitext(filepath)[1] != ".txt":
            message_emacs(message + ", disable lsp-bridge-mode.")
            eval_in_emacs("lsp-bridge--turn-off", filepath, get_lsp_file_host())

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
                    self.turn_off(filepath, error_message)
                else:
                    message_emacs(error_message)

                return False
        else:
            error_message = "Error: {}'s command argument is empty".format(filepath)

            if turn_off_on_error:
                self.turn_off(filepath, error_message)
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

    def maybe_create_org_babel_server(self, filepath):
        action = get_from_path_dict(FILE_ACTION_DICT, filepath)
        current_lang_server = get_emacs_func_result("get-single-lang-server",
                                                    action.single_server.project_path, filepath)
        lsp_server_name = "{}#{}".format(action.single_server.project_path, current_lang_server)
        if lsp_server_name != action.single_server.server_name and type(current_lang_server) is str:
            if lsp_server_name not in action.org_lang_servers:
                lang_server_info = load_single_server_info(current_lang_server)
                server = self.create_lsp_server(filepath, action.single_server.project_path,
                                                lang_server_info, enable_diagnostics=False)
                action.org_lang_servers[lsp_server_name] = server
                action.org_server_infos[lsp_server_name] = lang_server_info
                server.attach(action)
            else:
                lang_server_info = action.org_server_infos[lsp_server_name]
                server = action.org_lang_servers[lsp_server_name]
            action.single_server = server
            action.single_server_info = lang_server_info
            action.set_lsp_server()

    def build_file_action_function(self, name):
        def _do(filepath, *args):
            open_file_success = True

            if not is_in_path_dict(FILE_ACTION_DICT, filepath):
                open_file_success = self.open_file(filepath)  # _do is called inside event_loop, so we can block here.
            elif os.path.splitext(filepath)[-1] == '.org' and get_emacs_vars(['lsp-bridge-enable-org-babel'])[0]:
                # check weather need create new lsp server
                self.maybe_create_org_babel_server(filepath)

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

    def tabnine_complete(self, before, after, filename, region_includes_beginning, region_includes_end, max_num_results):
        self.tabnine.complete(before, after, filename, region_includes_beginning, region_includes_end, max_num_results)

    def codeium_complete(self, cursor_offset, editor_language, tab_size, text, insert_spaces, prefix, language):
        self.codeium.complete(cursor_offset, editor_language, tab_size, text, insert_spaces, prefix, language)

    def codeium_completion_accept(self, id):
        self.codeium.accept(id)

    def codeium_auth(self):
        self.codeium.auth()

    def codeium_get_api_key(self, auth_token):
        self.codeium.get_api_key(auth_token)

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
    lang_server_info = json.load(lang_server_path)

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

def get_lang_server_path(server_name):
    server_dir = Path(__file__).resolve().parent / "langserver"
    server_path_current = server_dir / "{}_{}.json".format(server_name, get_os_name())
    server_path_default = server_dir / "{}.json".format(server_name)

    user_server_dir = Path(str(get_emacs_vars(["lsp-bridge-user-langserver-dir"])[0])).expanduser()
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
