#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# /// script
# dependencies = [
#   "epc",
#   "orjson",
#   "packaging",
#   "sexpdata",
#   "six",
#   "setuptools",
#   "paramiko",
#   "rapidfuzz",
#   "watchdog",
# ]
# ///

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
import time

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
from core.search_paths import SearchPaths
from core.tabnine import TabNine
from core.codeium import Codeium
from core.copilot import Copilot
from core.utils import *
from core.handler import *
from core.remote_file import (
    RemoteFileClient,
    DockerFileClient,
    FileSyncServer,
    FileElispServer,
    FileCommandServer,
    SendMessageException,
    ContainerConnectionException,
    save_ip,
    get_container_local_ip
)
from core.ctags import Ctags

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
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
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

        self.sync_tramp_remote_complete_event = threading.Event()

        if self.running_in_server:
            self.init_search_backends_complete_event = threading.Event()
            # Start remote file server if lsp-bridge running in server.
            self.init_remote_file_server()
        else:
            # Init EPC client and search backends if lsp-bridge running in local.
            init_epc_client(int(args[0]))
            self.init_search_backends()
            eval_in_emacs('lsp-bridge--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def message_hostnames(self):
        message_emacs(f"host_names:{self.host_names}")

    # Functions for initialization
    def init_search_backends(self):
        # Init tabnine.
        self.tabnine = TabNine()

        # Init codeium
        self.codeium = Codeium()

        # Init copilot
        self.copilot = Copilot()

        # Init search backends.
        self.search_file_words = SearchFileWords()
        self.search_sdcv_words = SearchSdcvWords()
        self.search_list = SearchList()
        self.search_paths = SearchPaths()
        self.ctags = Ctags()

        # Build EPC interfaces.
        handler_subclasses = list(map(lambda cls: cls.name, Handler.__subclasses__()))
        for name in ["change_file", "update_file",  "save_file",
                     "try_completion", "try_formatting",
                     "change_cursor",
                     "list_diagnostics",
                     "try_code_action",
                     "workspace_symbol"] + handler_subclasses:
            self.build_file_action_function(name)

        search_backend_export_functions = {
            "search_file_words": ["index_files", "change_buffer", "load_file", "close_file", "search"],
            "search_sdcv_words": ["search"],
            "search_list": ["search", "update"],
            "search_paths": ["search"]
        }
        for search_backend, export_functions in search_backend_export_functions.items():
            for name in export_functions:
                self.build_prefix_function(search_backend, search_backend, name)

        [lsp_server_log_level] = get_emacs_vars(["lsp-bridge-log-level"])
        lsp_server_log_level = str(lsp_server_log_level)
        if lsp_server_log_level in ["debug", "warning", "error", "critical"]:
            log_level_dict = {
                "debug": logging.DEBUG,
                "warning": logging.WARNING,
                "error": logging.ERROR,
                "critical": logging.CRITICAL,
            }
            logger.setLevel(log_level_dict[lsp_server_log_level])

        # All LSP server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        if not self.running_in_server:
            remote_threads = {
                "send_message_dispatcher": {
                    # Build loop to open remote file.
                    "remote_file_sender_queue": REMOTE_FILE_SYNC_CHANNEL,
                    # Build loop to call remote Python command.
                    "remote_file_command_sender_queue": REMOTE_FILE_COMMAND_CHANNEL,
                    # Build loop to reply remote rpc command.
                    "remote_file_elisp_sender_queue": REMOTE_FILE_ELISP_CHANNEL
                },
                "receive_message_dispatcher": {
                    # Build loop to receive remote file to local Emacs.
                    "remote_file_receiver_queue": "handle_remote_file_message",
                    # Build loop to receive remote Python command response from remote server to local Emacs.
                    "remote_file_command_receiver_queue": "handle_lsp_message",
                    # Build loop to receive remote rpc from remote server to local Emacs.
                    "remote_file_elisp_receiver_queue": "handle_file_elisp_message"
                }
            }

            for dispatcher, args in remote_threads.items():
                for q, v in args.items():
                    newq = queue.Queue()
                    setattr(self, q, newq)
                    if not isinstance(v, int):
                        v = getattr(self, v)
                    threading.Thread(
                        target=getattr(self, dispatcher),
                        args=(newq, v)
                    ).start()

    @threaded
    def init_remote_file_server(self):
        print("* Running lsp-bridge on remote server. "
              "Access files with 'lsp-bridge-open-remote-file' or 'find-file /docker:...'")

        # Build loop for remote files management.
        self.file_server = FileSyncServer("0.0.0.0", REMOTE_FILE_SYNC_CHANNEL)

        # Build loop for call remote command from local Emacs.
        # Start waiting for init_search_backends_complete_event
        self.file_command_server = FileCommandServer("0.0.0.0", REMOTE_FILE_COMMAND_CHANNEL, self)

        # Build loop for call local Emacs function from server.
        # Signal that init_search_backends_complete_event is done
        self.file_elisp_server = FileElispServer("0.0.0.0", REMOTE_FILE_ELISP_CHANNEL, self)

        set_lsp_bridge_server(self)

    # Functions for communication between local and remote server
    def get_socket_client(self, server_host, server_port, is_retry=False):
        # currently only support SSH and docker
        #
        # try to distinguish SSH remote file or docker remote file using server_host
        # if visiting docker remote file, container name is passed as server_host
        if is_valid_ip(server_host):
            return self._get_remote_file_client(server_host, server_port, is_retry)
        else:
            # server_host is the container_name
            return self._get_docker_file_client(server_host, server_port)

    def _get_remote_file_client(self, server_host, server_port, is_retry):
        if server_host not in self.host_names:
            message_emacs(f"{server_host} is not connected, try reconnect...")
            self.sync_tramp_remote_complete_event.clear()
            eval_in_emacs('lsp-bridge-remote-reconnect', server_host, True)
            self.sync_tramp_remote_complete_event.wait()
            message_emacs(f"{server_host} connected.")

        client_id = f"{server_host}:{server_port}"
        if client_id in self.client_dict:
            return self.client_dict[client_id]

        import paramiko

        ssh_conf = self.host_names[server_host]
        try:
            client = RemoteFileClient(
                ssh_conf,
                server_port,
                lambda message: self.receive_remote_message(message, server_port),
            )
        except paramiko.AuthenticationException:
            # cloud not login server
            message_emacs(f"login {ssh_conf} failed, please check *lsp-bridge*")
            return None

        try:
            client.create_channel()
        except paramiko.ChannelException:
            # Channel Exception indicates that we clould not established channel
            # the remote process may not exist, try to start the process
            if is_retry:
                return None

            [remote_start_automatically] = get_emacs_vars(["lsp-bridge-remote-start-automatically"])
            if not remote_start_automatically:
                message_emacs(f"please make sure `lsp_bridge.py` has start at server {server_host} (lsp-bridge-remote-start-automatically is disabled)")
                return None
            # try to start remote process if it does not exist
            message_emacs(f"Start lsp-bridge process on {server_host} automatically...")
            client.start_lsp_bridge_process()
            # wait a while for the remote process to be ready
            time.sleep(2)
            # if client is not None, it has been started and put into client_dict
            return self.get_socket_client(server_host, server_port, is_retry=True)
        else:
            client.start()
            self.client_dict[client_id] = client
            return client

    def _get_docker_file_client(self, container_name, server_port):
        client_id = f"{container_name}:{server_port}"
        if client_id in self.client_dict:
            return self.client_dict[client_id]

        try:
            client = DockerFileClient(
                container_name=container_name,
                server_port=server_port,
                callback=lambda message: self.receive_remote_message(message, server_port),
            )
        except ContainerConnectionException as e:
            print(f"Failed to connect {container_name}, is it running?")
            raise e
        else:
            client.start()
            self.client_dict[client_id] = client
            return client

    def send_remote_message(self, host, queue, message, wait=False):
        queue.put({"host": host, "message": message})
        if wait:
            queue.join()

    def send_message_dispatcher(self, queue, port):
        try:
            while True:
                data = queue.get(True)

                server_host = data["host"]
                client = self.get_socket_client(server_host, port)
                try:
                    client.send_message(data["message"])
                except SendMessageException as e:
                    # lsp-bridge process might has been restarted, making the orignal socket no longer valid.
                    logger.exception("Connection %s is broken, message %s, error %s", f"{server_host}:{port}", data["message"], e)
                    # remove all the clients for server_host from client_dict
                    # client will be created again when get_socket_client is called
                    for client_id in [key for key in self.client_dict.keys() if key.startswith(server_host + ":")]:
                        self.client_dict.pop(client_id, None)

                    message_emacs(f"try to recreate connection to {server_host}:{port}")
                    try:
                        client = self.get_socket_client(server_host, port)
                    except Exception as e:
                        # FATAL: unable to restore
                        logger.exception(e)
                    else:
                        # connection restored, try to send out the message
                        client.send_message(data["message"])
                        eval_in_emacs('lsp-bridge-remote-reconnect', server_host, False)
                except Exception as e:
                    logger.exception(e)
                finally:
                    queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def receive_remote_message(self, message, server_port):
        if server_port == REMOTE_FILE_SYNC_CHANNEL:
            self.remote_file_receiver_queue.put(message)
        elif server_port == REMOTE_FILE_COMMAND_CHANNEL:
            self.remote_file_command_receiver_queue.put(message)
        elif server_port == REMOTE_FILE_ELISP_CHANNEL:
            self.remote_file_elisp_receiver_queue.put(message)

    def receive_message_dispatcher(self, queue, handle_remote_message):
        try:
            while True:
                message = queue.get(True)
                handle_remote_message(message)
                queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def remote_sync(self, host, remote_connection_info):
        client_id = f"{host}:{REMOTE_FILE_ELISP_CHANNEL}"
        if client_id not in self.client_dict:
            # send "say hello" upon establishing the first connection
            self.send_remote_message(
                host, self.remote_file_elisp_sender_queue, "Connect", True)

            self.send_remote_message(
                host, self.remote_file_sender_queue, {
                    "command": "remote_sync",
                    "server": host,
                    "remote_connection_info": remote_connection_info,
                })

    @threaded
    def sync_tramp_remote(self, tramp_file_name, tramp_method, server_username, server_host, ssh_port, path):

        # tramp_file_name format example:
        #   /ssh:user@ip#port:/path/to/file
        #   /docker:user@container:/path/to/file
        # see https://www.gnu.org/software/tramp/#File-name-syntax
        tramp_method_prefix = tramp_file_name.rsplit(":", 1)[0]

        if tramp_method_prefix.startswith("/ssh"):

            # arguments are passed from emacs using standard TRAMP functions tramp-file-name-<field>
            if server_host in self.host_names:
                server_host = self.host_names[server_host]['hostname']
                ssh_conf = self.host_names[server_host]
            elif is_valid_ip(server_host):
                ssh_conf = {'hostname' : server_host}
            else:
                import paramiko
                alias = server_host
                ssh_config = paramiko.SSHConfig()
                with open(os.path.expanduser('~/.ssh/config')) as f:
                    ssh_config.parse(f)
                ssh_conf = ssh_config.lookup(alias)

                server_host = ssh_conf.get('hostname', server_host)
                self.host_names[alias] = ssh_conf

            if not is_valid_ip(server_host):
                message_emacs("HostName Must be IP format.")

            if server_username:
                ssh_conf['user'] = server_username
            if ssh_port:
                ssh_conf['port'] = ssh_port
            self.host_names[server_host] = ssh_conf

            tramp_connection_info = tramp_method_prefix + ":"
            self.remote_sync(server_host, tramp_connection_info)

            eval_in_emacs("lsp-bridge-update-tramp-file-info", tramp_file_name, tramp_connection_info, tramp_method, server_username, server_host, path)
            self.sync_tramp_remote_complete_event.set()

        elif tramp_method_prefix.startswith("/docker"):
            # when using tramp to connect to container, the tramp_file_name might be
            #
            # /docker:USER@CONTAINER:/path/to/file
            # /docker:CONTAINER:/path/to/file
            #
            # see https://git.savannah.gnu.org/git/tramp.git listp/tramp-container.el
            tramp_method_split = tramp_method_prefix.split(":")[1]
            if "@" in tramp_method_split:
                container_user, container_name = tramp_method_split.split("@")
            else:
                container_user = "root"
                container_name = tramp_method_split

            if not get_container_local_ip(container_name):
                message_emacs(f"Cloud not get local ip of container {container_name}, is it running?")
                return

            # only support local container
            self.host_names[container_name] = {
                "server_host": "127.0.0.1",
                "username": container_user
            }

            try:
                tramp_connection_info = tramp_method_prefix + ":"
                self.remote_sync(container_name, tramp_connection_info)
                # container_name is used for emacs buffer local variable lsp-bridge-remote-file-host
                eval_in_emacs("lsp-bridge-update-tramp-file-info", tramp_file_name, tramp_connection_info, tramp_method, container_user, container_name, path)
                eval_in_emacs("lsp-bridge--setup-tramp-docker-buffer", tramp_file_name)
                self.sync_tramp_remote_complete_event.set()
            except Exception as e:
                logger.exception(e)
                message_emacs(f"Connect {container_user}@{container_name} failed")
                raise e
        else:
            message_emacs(
                f"Failed to access {tramp_file_name}, unsupported tramp methd: {tramp_method_prefix[1:]}, "
                "please make sure `lsp_bridge.py` has started on server."
            )

    # Functions for local to manage remote files
    @threaded
    def open_remote_file(self, path, jump_define_pos):
        if is_valid_ip_path(path):
            # open_remote_file path notation
            # ip:path
            # ip:port:path
            path_info = split_ssh_path(path)
            if path_info:
                (remote_info, server_host, ssh_conf, server_path) = path_info

                self.host_names[server_host] = ssh_conf

                self.remote_sync(server_host, f"/{remote_info}")

                message_emacs(f"Open {remote_info}{server_path}...")
                # Add TRAMP-related fields
                # The following fields: tramp_method, user, server, port, and path
                # are set as buffer-local variables in the buffer created by Emacs.
                # These variables facilitate the construction of a TRAMP file name,
                # and then allow the buffer to reconnect to a restarted remote lsp-bridge process
                # using the same logic with reconnecting a TRAMP remote file buffer.
                self.send_remote_message(
                    server_host, self.remote_file_sender_queue, {
                    "command": "open_file",
                    "tramp_method": "ssh",
                    "user": ssh_conf.get('user'),
                    "server": server_host,
                    "port": ssh_conf.get('port'),
                    "path": server_path,
                    "jump_define_pos": epc_arg_transformer(jump_define_pos)
                })
                save_ip(f"{remote_info}")
        elif path.startswith("/docker:"):
            # open_remote_file path notation
            # /docker:user@container:/path/to/file
            # /docker:container:/path/to/file
            path_info = split_docker_path(path)
            if path_info:
                (container_user, server_host, server_path) = path_info

                # only support local container
                self.host_names[server_host] = {
                    "server_host": "127.0.0.1",
                    "username": container_user
                }

                self.remote_sync(server_host, path.rsplit(":", 1)[0] + ":")

                message_emacs(f"Open {path}...")

                self.send_remote_message(
                    server_host, self.remote_file_sender_queue, {
                        "command": "open_file",
                        "tramp_method": "docker",
                        "user": container_user,
                        "server": server_host,
                        "port": None,
                        "path": server_path,
                        "jump_define_pos": epc_arg_transformer(jump_define_pos)
                })
        else:
            message_emacs(f"Unsupported remote path {path}")

    @threaded
    def update_remote_file(self, remote_file_host, remote_file_path, content):
        self.send_remote_message(
            remote_file_host, self.remote_file_sender_queue, {
            "command": "update_file",
            "server": remote_file_host,
            "path": remote_file_path,
            "content": content
        })

    @threaded
    def save_remote_file(self, remote_file_host, remote_file_path):
        self.send_remote_message(
            remote_file_host, self.remote_file_sender_queue, {
            "command": "save_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

    @threaded
    def close_remote_file(self, remote_file_host, remote_file_path):
        self.send_remote_message(
            remote_file_host, self.remote_file_sender_queue, {
            "command": "close_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

    # Functions for local to send request to remote server
    @threaded
    def lsp_request(self, remote_file_host, remote_file_path, method, args):
        if method == "change_file":
            self.send_remote_message(
                remote_file_host, self.remote_file_sender_queue, {
                "command": "change_file",
                "server": remote_file_host,
                "path": remote_file_path,
                "args": list(map(epc_arg_transformer, args))
            })

        self.send_remote_message(
            remote_file_host, self.remote_file_command_sender_queue, {
            "command": "lsp_request",
            "server": remote_file_host,
            "path": remote_file_path,
            "method": method,
            "args": list(map(epc_arg_transformer, args))
        })

    @threaded
    def func_request(self, remote_file_host, remote_file_path, method, args):
        self.send_remote_message(
            remote_file_host, self.remote_file_command_sender_queue, {
            "command": "func_request",
            "server": remote_file_host,
            "path": remote_file_path,
            "method": method,
            "args": list(map(epc_arg_transformer, args))
        })

    # Functions for local to handle messages from remote server
    @threaded
    def handle_remote_file_message(self, message):
        command = message["command"]
        if command == "open_file":
            if "error" in message:
                message_emacs(message["error"])
            else:
                eval_in_emacs(
                    "lsp-bridge-open-remote-file--response",
                    message["tramp_method"],
                    message["user"],
                    message["server"],
                    message["port"],
                    message["path"],
                    string_to_base64(message["content"]),
                    message["jump_define_pos"],
                )
                message_emacs(f"Open file {message['path']} on {message['server']}")

    @threaded
    def handle_lsp_message(self, message):
        if message["command"] == "eval-in-emacs":
            # Execute emacs command from remote server.
            eval_sexps_in_emacs(message["sexp"])

    @threaded
    def handle_file_elisp_message(self, message):
        # Receive elisp RPC call from remote server.
        log_time(f"Receive server elisp RPC: {message}")

        host = message["host"]

        # Read elisp code from local Emacs, and sendback to remote server.
        if message["command"] == "get_emacs_func_result":
            result = get_emacs_func_result(message["method"], *message["args"])
        elif message["command"] == "get_emacs_vars":
            result = get_emacs_vars(message["args"])
        else:
            logger.error("Unsupported command %s", message["command"])
            result = None

        message["result"] = result
        self.send_remote_message(host, self.remote_file_elisp_sender_queue, message)

    # Functions for local handling
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

        # notify change workspace folder to copilot server
        if self.copilot.is_initialized:
            self.copilot.change_workspace_folder(project_path)

        if os.path.splitext(filepath)[-1] == '.org':
            single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)
            lang_server_info = load_single_server_info(single_lang_server)
            #TODO support diagnostic
            lsp_server = self.create_lsp_server(filepath, project_path, lang_server_info, enable_diagnostics=False)
            create_file_action_with_single_server(filepath, lang_server_info, lsp_server)
        elif multi_lang_server:
            # Try to load multi language server when get-multi-lang-server return match one.
            multi_lang_server_path = get_lang_server_path(multi_lang_server, True)

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

    def close_all_files(self):
        FILE_ACTION_DICT.clear()

        for lsp_server in LSP_SERVER_DICT.values():
            lsp_server.exit()
        LSP_SERVER_DICT.clear()

    def enjoy_hacking(self, servers, project_path):
        # Notify user server is ready.
        print("Start lsp server ({}) for {}".format(", ".join(servers), project_path))

        message_emacs("Active {} '{}', enjoy hacking!".format(
            "project" if os.path.isdir(project_path) else "file",
            os.path.basename(project_path.rstrip(os.path.sep))))

    def load_single_lang_server(self, project_path, filepath):
        single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)

        if not single_lang_server:
            self.turn_off(filepath, "ERROR: can't find the corresponding server for {}".format(filepath))

            return False

        lang_server_info = load_single_server_info(single_lang_server)

        # If project_path is file path, not dir.
        if ((not os.path.isdir(project_path)) and
            ("support-single-file" in lang_server_info and
             lang_server_info["support-single-file"] is False)):

            if "projectFiles" in lang_server_info:
                # If support-support-single-file is False,
                # we will search projectFiles up 20 level directories to find project root.
                project_root = self.find_project_root(filepath, lang_server_info["projectFiles"])
                if project_root is None:
                    self.turn_off_by_single_file(filepath, single_lang_server)

                    return False
                else:
                    # Replace project_path with project root path.
                    project_path = project_root
            else:
                self.turn_off_by_single_file(filepath, single_lang_server)

                return False

        lsp_server = self.create_lsp_server(filepath, project_path, lang_server_info)

        if lsp_server:
            self.enjoy_hacking([lang_server_info["name"]], project_path)

            create_file_action_with_single_server(filepath, lang_server_info, lsp_server)
        else:
            return False

        return True

    def find_project_root(self, filepath, project_files, max_depth=20):
        current_dir = os.path.abspath(filepath)

        for _ in range(max_depth):
            for file in project_files:
                if os.path.isfile(os.path.join(current_dir, file)):
                    return current_dir

            parent_dir = os.path.dirname(current_dir)
            if parent_dir == current_dir: # reach topest dir
                break

            current_dir = parent_dir

        return None

    def turn_off(self, filepath, message):
        if os.path.splitext(filepath)[1] != ".txt":
            message_emacs(message + ", disable LSP feature.")
            eval_in_emacs("lsp-bridge--turn-off-lsp-feature", filepath, get_lsp_file_host())

    def turn_off_by_single_file(self, filepath, single_lang_server):
        self.turn_off(
            filepath,
            "ERROR: {} not support single-file, you need put this file in a git repository or put .dir-locals.el in project root directory".format(single_lang_server))

    def check_lang_server_command(self, lang_server_info, filepath, turn_off_on_error=True):
        # We merge PATH from `exec-path` variable, to make sure lsp-bridge find LSP server command if it can find by Emacs.
        merge_emacs_exec_path()

        if len(lang_server_info["command"]) > 0:
            server_command = lang_server_info["command"][0]
            server_command_path = shutil.which(server_command)

            if server_command_path:
                # We always replace LSP server command with absolute path of 'which' command.
                lang_server_info["command"][0] = server_command_path
                # message back the LSP server command path to emacs for verification
                # as some languages have runtime environment isolation
                # for example python virtualenv, NodeJS nvm, Ruby RVM
                message_emacs(f"found language server: {server_command_path}")
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
            if isinstance(info_value, str):
                servers.append(info_value)
            else:
                servers += info_value

        return list(dict.fromkeys(servers))

    def maybe_create_org_babel_server(self, filepath):
        action = get_from_path_dict(FILE_ACTION_DICT, filepath)
        current_lang_server = get_emacs_func_result("get-single-lang-server",
                                                    action.single_server.project_path, filepath)
        lsp_server_name = "{}#{}".format(action.single_server.project_path, current_lang_server)
        if lsp_server_name != action.single_server.server_name and isinstance(current_lang_server, str):
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
            if is_remote_path(filepath):
                return
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
            # To prevent long-time calculations from blocking Emacs, we need to put the function into event loop.
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

    @threaded
    def ctags_complete(self, symbol, filename, max_lines, cursor_offset):
        self.ctags.make_complete(symbol, filename, max_lines, cursor_offset)

    @threaded
    def ctags_find_def(self, symbol, filename):
        self.ctags.find_definition(symbol, filename)

    def copilot_complete(self, position, editor_mode, file_path, relative_path, tab_size, text, insert_spaces):
        self.copilot.complete(position, editor_mode, file_path, relative_path, tab_size, text, insert_spaces)

    @threaded
    def codeium_complete(self, cursor_offset, editor_language, tab_size, text, insert_spaces, prefix, language, file_path):
        self.codeium.complete(cursor_offset, editor_language, tab_size, text, insert_spaces, prefix, language, file_path)

    def codeium_completion_accept(self, id):
        self.codeium.accept(id)

    def codeium_auth(self):
        self.codeium.auth()

    def copilot_login(self):
        self.copilot.login()

    def copilot_logout(self):
        self.copilot.logout()

    def copilot_status(self):
        self.copilot.check_status()

    def copilot_completion_accept(self, id):
        self.copilot.accept(id)

    def codeium_get_api_key(self, auth_token):
        self.codeium.get_api_key(auth_token)

    def handle_server_process_exit(self, server_name):
        if server_name in LSP_SERVER_DICT:
            log_time("Exit server {}".format(server_name))
            del LSP_SERVER_DICT[server_name]

    def close_client(self):
        for client in self.client_dict.values():
            if hasattr(client, "kill_lsp_bridge_process"):
                client.kill_lsp_bridge_process()

    def cleanup(self):
        """Do some cleanup before exit python process."""
        self.close_client()
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
    lang_server_info["command"] = eval(replace_template(str(lang_server_info["command"])))

    # Replace template in initializationOptions.
    if "initializationOptions" in lang_server_info:
        lang_server_info["initializationOptions"] = eval(replace_template(str(lang_server_info["initializationOptions"])))

    return lang_server_info

def load_single_server_info(lang_server):
    lang_server_info_path = ""
    if isinstance(lang_server, str) and os.path.exists(lang_server) and os.path.dirname(lang_server) != "":
        # If lang_server is real file path, we load the LSP server configuration from the user specified file.
        lang_server_info_path = lang_server
    else:
        # Otherwise, we load LSP server configuration from file lsp-bridge/langserver/lang_server.json.
        lang_server_info_path = get_lang_server_path(lang_server)

    with open(lang_server_info_path, encoding="utf-8", errors="ignore") as f:
        return read_lang_server_info(f)

def get_lang_server_path(server_name, is_multi_server=False):
    lang_server_dir = "multiserver" if is_multi_server else "langserver"
    lang_server_dir_var = "lsp-bridge-user-multiserver-dir" if is_multi_server else "lsp-bridge-user-langserver-dir"

    server_dir = Path(__file__).resolve().parent / lang_server_dir
    server_path_current = server_dir / "{}_{}.json".format(server_name, get_os_name())
    server_path_default = server_dir / "{}.json".format(server_name)

    user_server_dir = Path(str(get_emacs_vars([lang_server_dir_var])[0])).expanduser()
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
