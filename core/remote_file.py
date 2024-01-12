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
from core.utils import *


class SendMessageException(Exception):
    pass


class RemoteFileClient(threading.Thread):
    remote_password_dict = {}

    def __init__(self, ssh_host, ssh_user, ssh_port, server_port, callback, use_gssapi=False, proxy_command=None):
        threading.Thread.__init__(self)

        # Init.
        self.ssh_host = ssh_host
        self.ssh_user = ssh_user
        self.ssh_port = ssh_port
        self.server_port = server_port
        self.callback = callback

        [self.user_ssh_private_key] = get_emacs_vars(["lsp-bridge-user-ssh-private-key"])

        self.ssh = self.connect_ssh(use_gssapi, proxy_command)
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

    def send_message(self, message):
        """Send message via the channel

        :raises: :class:`SendMessageException`: if channel is invalid
        """
        try:
            data = json.dumps(message)
            self.chan.sendall(f"{data}\n".encode("utf-8"))
        except socket.error as e:       
            raise SendMessageException() from e

    def run(self):
        chan_file = self.chan.makefile("r")
        while True:
            message = chan_file.readline().strip()
            if not message:
                break
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
            if ! pidof {remote_python_command} >/dev/null 2>&1; then
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

        # Build message loop.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        self.file_dict = {}

    def event_dispatcher(self):
        try:
            while True:
                client_socket, client_address = self.server.accept()

                client_handler = threading.Thread(target=self.handle_client, args=(client_socket,))
                client_handler.start()
        except:
            print(traceback.format_exc())

    def message_dispatcher(self):
        try:
            while True:
                client_socket = self.message_queue.get(True)
                self.handle_client(client_socket)
                self.message_queue.task_done()
        except:
            print(traceback.format_exc())

    def handle_client(self, client_socket):
        client_file = client_socket.makefile('r')
        while True:
            message = client_file.readline().strip()
            if not message:
                break
            self.handle_message(message, client_socket)
        client_socket.close()

    def handle_message(self, message, client_socket):
        data = parse_json_content(message)
        command = data["command"]

        if command == "open_file":
            self.handle_open_file(data, client_socket)
        elif command == "save_file":
            self.handle_save_file(data, client_socket)
        elif command == "close_file":
            self.handle_close_file(data, client_socket)
        elif command == "change_file":
            self.handle_change_file(data, client_socket)
        elif command == "tramp_sync":
            self.handle_tramp_sync(data, client_socket)

    def handle_tramp_sync(self, data, client_socket):
        tramp_connection_info = data["tramp_connection_info"]
        set_remote_tramp_connection_info(tramp_connection_info)

    def handle_open_file(self, data, client_socket):
        path = os.path.expanduser(data["path"])
        response = {**data, "path": path}

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

        response_data = json.dumps(response)
        client_socket.send(f"{response_data}\n".encode("utf-8"))

    def handle_change_file(self, data, client_socket):
        path = data["path"]
        if path not in self.file_dict:
            with open(path) as f:
                self.file_dict[path] = f.read()

        self.file_dict[path] = rebuild_content_from_diff(self.file_dict[path], data["args"][0], data["args"][1], data["args"][3])

    def handle_save_file(self, data, client_socket):
        path = data["path"]

        if path in self.file_dict:
            with open(path, 'w') as file:
                file.write(self.file_dict[path])

    def handle_close_file(self, data, client_socket):
        path = data["path"]

        if path in self.file_dict:
            del self.file_dict[path]


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
