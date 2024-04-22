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

import threading
import os
import glob
import json
import socket
import traceback
import time
from core.utils import *


class SendMessageException(Exception):
    pass


class RemoteFileClient(threading.Thread):
    remote_password_dict = {}

    def __init__(self, ssh_conf, server_port, callback):
        threading.Thread.__init__(self)

        # Init.
        self.ssh_host = ssh_conf['hostname']
        self.ssh_user = ssh_conf.get('user', "root")
        self.ssh_port = ssh_conf.get('port', 22)
        self.server_port = server_port
        self.callback = callback

        [self.user_ssh_private_key] = get_emacs_vars(["lsp-bridge-user-ssh-private-key"])

        self.ssh = self.connect_ssh(
            ssh_conf.get('gssapiauthentication', 'no') in ('yes'),
            ssh_conf.get('proxycommand', None)
        )
        # after successful login, don't create a channel yet
        # caller can use client ssh to execute command on remote server
        # ande then call create_channel() to create the channel
        self.chan = None

    def ssh_private_key(self):
        """Retrieves the path to the SSH private key file.

        The user can specify the SSH private key path by setting the
        `lsp-bridge-user-ssh-private-key` in emacs.

        If this configuration is not set, the function defaults to using the
        first found public key to determine the private key file in the `.ssh`
        directory.
        """
        if not self.user_ssh_private_key:
            ssh_dir = "~/.ssh"
            ssh_dir = os.path.expanduser(ssh_dir)
            pub_keys = glob.glob(os.path.join(ssh_dir, "*.pub"))
            default_pub_key = pub_keys[0]
            private_key = default_pub_key[: -len(".pub")]
        else:
            private_key = os.path.expanduser(self.user_ssh_private_key)
        return private_key

    def connect_ssh(self, use_gssapi, proxy_command):
        """Connect to remote ssh_host

        :raises: :class:`paramiko.AuthenticationException`: if all authentication method failed
        """
        import paramiko

        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

        proxy = None
        if proxy_command:
            proxy = paramiko.ProxyCommand(proxy_command)

        try:
            if use_gssapi:
                ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, gss_auth=True, gss_kex=True, sock=proxy)
            else:
                # Login server with ssh private key.
                ssh_private_key = self.ssh_private_key()
                # look_for_keys defaults to True
                # when user specify the SSH private key path
                # disable searching for discoverable private key files in ~/.ssh/
                look_for_keys = not self.user_ssh_private_key
                ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, key_filename=ssh_private_key, look_for_keys=look_for_keys, sock=proxy)
        except:
            print(traceback.format_exc())

            # Try login server with password if private key is not available.
            try:
                # running `get-ssh-password` on macOS will raise error of 'The macOS Keychain auth-source backend doesn’t support creation yet'
                password = RemoteFileClient.remote_password_dict[self.ssh_host] if self.ssh_host in RemoteFileClient.remote_password_dict else get_ssh_password(self.ssh_user, self.ssh_host, self.ssh_port)

                ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, password=password)

                # Only remeber server's login password after login server successfully.
                # Password only record in memory for session login, not save in file.
                RemoteFileClient.remote_password_dict[self.ssh_host] = password
            except:
                print(traceback.format_exc())
                raise paramiko.AuthenticationException()

        return ssh

    def create_channel(self):
        """Create channel to lsp-bridge process running in server

        :raises: :class:`paramiko.ChannelException`: if server lsp-bridge process doesn't exisit
        """
        self.chan = self.ssh.get_transport().open_channel(
            "direct-tcpip", (self.ssh_host, self.server_port), ("0.0.0.0", 0)
        )
        if self.chan:
            [self.remote_heartbeat_interval] = get_emacs_vars(["lsp-bridge-remote-heartbeat-interval"])
            if self.remote_heartbeat_interval and self.remote_heartbeat_interval != 0:
                threading.Thread(target=self.heartbeat).start()

    def heartbeat(self):
        try:
            while True:
                self.chan.sendall("ping\n".encode("utf-8"))
                log_time_debug(f"Ping server: {self.ssh_host}, port: {self.server_port}")
                time.sleep(self.remote_heartbeat_interval)
        except Exception as e:
            logger.exception(e)

    def send_message(self, message):
        """Send message via the channel

        :raises: :class:`SendMessageException`: if channel is invalid
        """
        try:
            data = json.dumps(message)
            self.chan.sendall(f"{data}\n".encode("utf-8"))
        except socket.error as e:
            raise SendMessageException() from e
        else:
            log_time_debug(f"Sended to server {self.ssh_host} port {self.server_port}: {message}")

    def run(self):
        chan_file = self.chan.makefile("r")
        while True:
            data = chan_file.readline().strip()
            if not data:
                break

            message = parse_json_content(data)
            log_time_debug(f"Received from server {self.ssh_host} port {self.server_port}: {message}")
            self.callback(message)
        self.chan.close()

    def start_lsp_bridge_process(self):
        [remote_python_command] = get_emacs_vars(["lsp-bridge-remote-python-command"])
        [remote_python_file] = get_emacs_vars(["lsp-bridge-remote-python-file"])
        [remote_log] = get_emacs_vars(["lsp-bridge-remote-log"])

        # use -l option to bash as a login shell, ensuring that login scripts (like ~/.bash_profile) are read and executed.
        # This is useful for lsp-bridge to use environment settings to correctly find out language server command
        _, stdout, stderr = self.ssh.exec_command(
            f"""
            nohup /bin/bash -l -c '
            pid=$(ps aux | grep -v grep | grep lsp_bridge.py | cut -d " " -f2)
            if [ "$pid" == "" ]; then
                echo -e "Start lsp-bridge process as user $(whoami)" | tee >{remote_log}
                {remote_python_command} {remote_python_file} >>{remote_log} 2>&1 &
                if [ "$?" = "0" ]; then
                    echo -e "Start lsp-bridge successfully" | tee >>{remote_log}
                else
                    echo -e "Start lsp-bridge failed" | tee >>{remote_log}
                fi
            fi'
        """
        )
        print(f"Remote process started at {self.ssh_host}")
        print("stdout:" + stdout.read().decode())
        print("stderr:" + stderr.read().decode())


class RemoteFileServer:
    def __init__(self, host, port):
        import socket

        # Init.
        self.host = host
        self.port = port
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.server.bind((self.host, self.port))
        self.server.listen(5)

        # Build event loop.
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.client_socket = None
        self.client_address = None

    def event_dispatcher(self):
        try:
            while True:
                self.client_socket, self.client_address = self.server.accept()

                threading.Thread(target=self.handle_client).start()
        except Exception as e:
            logger.exception(e)

    def handle_client(self):
        try:
            client_file = self.client_socket.makefile('r')
            while True:
                data = client_file.readline().strip()
                if not data:
                    break
                elif data == "ping":
                    log_time_debug(f"Server port {self.port} received ping from client {self.client_address}")
                    continue

                message = parse_json_content(data)
                log_time_debug(f"Server port {self.port} received message from client {self.client_address}: {message}")
                resp = self.handle_message(message)
                if resp:
                    self.client_socket.send(f"{resp}\n".encode("utf-8"))

            client_file.close()
            self.client_socket.shutdown(socket.SHUT_RDWR)
            self.client_socket.close()
            log_time(f"Server port {self.port} socket close for client {self.client_address}")
            self.client_socket = None
            self.client_address = None
        except Exception as e:
            logger.exception(e)

    def handle_message(self, message):
        return

    def send_message(self, message):
        try:
            if self.client_socket:
                if self.client_address:
                    message["host"] = self.client_address[0]

                data = json.dumps(message)
                self.client_socket.send(f"{data}\n".encode("utf-8"))
        except Exception as e:
            logger.exception(e)
            raise SendMessageException() from e
        else:
            log_time_debug(f"Server port {self.port} sended to client {self.client_address}: {message}")


class FileSyncServer(RemoteFileServer):
    def __init__(self, host, port):
        self.file_dict = {}
        super().__init__(host, port)

    def handle_message(self, message):
        command = message["command"]

        if command == "open_file":
            return self.handle_open_file(message)
        elif command == "save_file":
            return self.handle_save_file(message)
        elif command == "close_file":
            return self.handle_close_file(message)
        elif command == "change_file":
            return self.handle_change_file(message)
        elif command == "remote_sync":
            return self.handle_remote_sync(message)

    def handle_remote_sync(self, message):
        remote_info = message["remote_connection_info"]
        set_remote_connection_info(remote_info)

    def handle_open_file(self, message):
        path = os.path.expanduser(message["path"])
        response = {**message, "path": path}

        if os.path.exists(path):
            with open(path) as f:
                content = f.read()
                response.update({
                    "path": path,
                    "content": content
                })
                self.file_dict[path] = content
        else:
            response.update({
                "path": path,
                "content": "",
                "error": f"Cannot found file {path} on server.",
            })

        return json.dumps(response)

    def handle_change_file(self, message):
        path = message["path"]
        if path not in self.file_dict:
            with open(path) as f:
                self.file_dict[path] = f.read()

        self.file_dict[path] = rebuild_content_from_diff(self.file_dict[path], message["args"][0], message["args"][1], message["args"][3])

    def handle_save_file(self, message):
        path = message["path"]

        if path in self.file_dict:
            with open(path, 'w') as file:
                file.write(self.file_dict[path])

    def handle_close_file(self, message):
        path = message["path"]

        if path in self.file_dict:
            del self.file_dict[path]

    def close_all_files(self):
        self.file_dict.clear()


class FileElispServer(RemoteFileServer):
    def __init__(self, host, port, lsp_bridge):
        self.lsp_bridge = lsp_bridge
        self.result_queue = queue.Queue()
        super().__init__(host, port)

    def handle_client(self):
        # remote server lsp-bridge process use this cient_socket to call elisp function from local Emacs.
        log_time(f"Client connect from {self.client_address[0]}:{self.client_address[1]}")

        threading.Thread(target=super().handle_client).start()

        self.lsp_bridge.init_search_backends()
        log_time("init_search_backends finish")
        # Signal that init_search_backends is done
        self.lsp_bridge.init_search_backends_complete_event.set()

    def handle_message(self, message):
        if message == "Connect":
            # Drop "say hello" message from local Emacs.
            return
        else:
            self.result_queue.put(message)

    def call_remote_rpc(self, message):
        try:
            self.send_message(message)
        except Exception as e:
            logger.exception(e)
            return None
        else:
            result = self.result_queue.get()
            self.result_queue.task_done()
            return result


class FileCommandServer(RemoteFileServer):
    def __init__(self, host, port, lsp_bridge):
        self.lsp_bridge = lsp_bridge
        super().__init__(host, port)

    def handle_client(self):
        # Record server host when lsp-bridge running in remote server.
        # we wait for init_search_backends to finish execution
        # before start thread to handle remote request
        log_time("wait for init_search_backends to finsih execution")
        self.lsp_bridge.init_search_backends_complete_event.wait()

        super().handle_client()

        # close all files when client disconnects
        self.lsp_bridge.file_server.close_all_files()
        self.lsp_bridge.close_all_files()

    def handle_message(self, message):
        if message["command"] == "lsp_request":
            # Call LSP request.
            self.lsp_bridge.event_queue.put({
                "name": "action_func",
                "content": ("_{}".format(message["method"]), [message["path"]] + message["args"])
            })
        elif message["command"] == "func_request":
            # Call lsp-bridge normal function.
            getattr(self.lsp_bridge, message["method"])(*message["args"])


def save_ip_to_file(ip, filename):
    with open(filename, 'r') as f:
        existing_ips = f.read().splitlines()

    if ip in existing_ips:
        return

    existing_ips.append(ip)

    with open(filename, 'w') as f:
        f.write('\n'.join(existing_ips))


def save_ip(ip):
    user_emacs_dir = get_emacs_func_result("get-user-emacs-directory")
    ip_file = os.path.join(user_emacs_dir, "lsp-bridge", "remote_file", "ip.txt")
    touch(ip_file)

    save_ip_to_file(ip, ip_file)
