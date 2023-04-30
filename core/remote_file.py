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
import traceback
from core.utils import *

class RemoteFileClient(threading.Thread):

    remote_password_dict = {}

    def __init__(self, ssh_host, ssh_user, ssh_port, server_port, callback):
        threading.Thread.__init__(self)

        # Init.
        self.ssh_host = ssh_host
        self.ssh_user = ssh_user
        self.ssh_port = ssh_port
        self.server_port = server_port
        self.callback = callback


        # Build SSH channel between local client and remote server.
        self.ssh = self.connect_ssh()
        self.transport = self.ssh.get_transport()
        self.chan = self.transport.open_channel("direct-tcpip", (self.ssh_host, self.server_port), ('0.0.0.0', 0))

    def ssh_pub_key(self):
        # Read SSH public key.
        ssh_dir = os.path.expanduser('~/.ssh')
        pub_keys = glob.glob(os.path.join(ssh_dir, '*.pub'))
        return pub_keys[0]

    def connect_ssh(self):
        import paramiko
        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

        try:
            # Login server with ssh public key.
            pub_key = self.ssh_pub_key()
            ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, key_filename=pub_key)
        except:
            # Try login server with password if public key is not available.
            password = RemoteFileClient.remote_password_dict[self.ssh_host] if self.ssh_host in RemoteFileClient.remote_password_dict else get_ssh_password(self.ssh_host)
            try:
                ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, password=password)

                # Only remeber server's login password after login server successfully.
                # Password only record in memory for session login, not save in file.
                RemoteFileClient.remote_password_dict[self.ssh_host] = password
            except:
                pass

        return ssh

    def send_message(self, message):
        try:
            data = json.dumps(message)
            self.chan.sendall(f"{data}\n".encode("utf-8"))
        except:
            logger.error(traceback.format_exc())

    def run(self):
        chan_file = self.chan.makefile('r')
        while True:
            message = chan_file.readline().strip()
            if not message:
                break
            self.callback(message)
        self.chan.close()

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
        data = json.loads(message)
        command = data["command"]

        if command == "open_file":
            self.handle_open_file(data, client_socket)
        elif command == "save_file":
            self.handle_save_file(data, client_socket)
        elif command == "close_file":
            self.handle_close_file(data, client_socket)
        elif command == "change_file":
            self.handle_change_file(data, client_socket)

    def handle_open_file(self, data, client_socket):
        path = data["path"]
        server = data["server"]
        jump_define_pos = data["jump_define_pos"]

        if os.path.exists(path):
            with open(path) as f:
                content = f.read()

                response = {
                    "command": "open_file",
                    "server": server,
                    "path": path,
                    "jump_define_pos": jump_define_pos,
                    "content": content
                }

                self.file_dict[path] = content
        else:
            response = {
                "command": "open_file",
                "server": server,
                "path": path,
                "jump_define_pos": jump_define_pos,
                "content": "",
                "error": f"Cannot found file {path} on server."
            }

        response_data = json.dumps(response)
        client_socket.send(f"{response_data}\n".encode("utf-8"))

    def handle_change_file(self, data, client_socket):
        path = data["path"]
        if path not in self.file_dict:
            with open(path) as f:
                self.file_dict[path] = f.read()

        content = self.file_dict[path]

        # Synchronously file content in memory for edit big file non-block.
        start_line = data["args"][0]['line']
        start_char = data['args'][0]['character']
        end_line = data['args'][1]['line']
        end_char = data['args'][1]['character']

        start_pos = get_position(content, start_line, start_char)
        end_pos = get_position(content, end_line, end_char)

        content = content[:start_pos] + data['args'][3] + content[end_pos:]

        self.file_dict[path] = content

    def handle_save_file(self, data, client_socket):
        path = data["path"]

        if path in self.file_dict:
            with open(path, 'w') as file:
                file.write(self.file_dict[path])

    def handle_close_file(self, data, client_socket):
        path = data["path"]

        if path in self.file_dict:
            self.file_dict[path] = ""

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
