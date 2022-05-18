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

import json
import os
import queue
import re
import subprocess
import threading
import traceback
from subprocess import PIPE
from sys import stderr
from threading import Thread
from urllib.parse import urlparse
from typing import Set, Dict, TYPE_CHECKING

if TYPE_CHECKING:
    from core.fileaction import FileAction
from core.utils import *

DEFAULT_BUFFER_SIZE = 100000000  # we need make buffer size big enough, avoid pipe hang by big data response from LSP server


class LspServerSender(Thread):
    def __init__(self, process: subprocess.Popen):
        super().__init__()

        self.process = process
        self.init_queue = queue.Queue()
        self.queue = queue.Queue()
        self.initialized = threading.Event()

    def _enqueue_message(self, message: dict, *, init=False):
        message["jsonrpc"] = "2.0"
        if init:
            self.init_queue.put(message)
        else:
            self.queue.put(message)

    def send_request(self, method, params, request_id, **kwargs):
        self._enqueue_message(dict(
            id=request_id,
            method=method,
            params=params
        ), **kwargs)

    def send_notification(self, method, params, **kwargs):
        self._enqueue_message(dict(
            method=method,
            params=params
        ), **kwargs)

    def send_response(self, request_id, result, **kwargs):
        self._enqueue_message(dict(
            id=request_id,
            result=result
        ), **kwargs)

    def _send_message(self, message: dict):
        message_str = "Content-Length: {}\r\n\r\n{}".format(len(json.dumps(message)), json.dumps(message))

        self.process.stdin.write(message_str.encode("utf-8"))
        self.process.stdin.flush()

        logger.info("\n--- Send ({}): {}".format(
            message.get('id', 'notification'), message.get('method', 'response')))

        logger.debug(json.dumps(message, indent=3))

    def run(self) -> None:
        # send "initialize" request
        self._send_message(self.init_queue.get())
        # wait until initialized
        self.initialized.wait()
        # send other initialization-related messages
        while not self.init_queue.empty():
            message = self.init_queue.get()
            self._send_message(message)
        # send all others
        while self.process.poll() is None:
            message = self.queue.get()
            self._send_message(message)


class LspServerReceiver(Thread):

    def __init__(self, process: subprocess.Popen):
        Thread.__init__(self)

        self.process = process
        self.queue = queue.Queue()

    def get_message(self):
        return self.queue.get(block=True)

    def emit_message(self, line):
        if not line:
            return
        try:
            # Send message.
            self.queue.put({
                "name": "lsp_recv_message",
                "content": json.loads(line)
            })
        except:
            logger.error(traceback.format_exc())

    def run(self):
        content_length = None
        buffer = bytearray()
        while self.process.poll() is None:
            try:
                if content_length is None:
                    match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                    if match is not None:
                        end = match.end()
                        parts = match.group(0).decode("utf-8").strip().split(": ")
                        content_length = int(parts[1])

                        buffer = buffer[end:]
                    else:
                        line = self.process.stdout.readline()
                        # dart_analysis_server 会发送 Content-Type,
                        # 导致解析的 json 回包内容不完整
                        if re.search(b"Content-Type", line) is None:
                            buffer = buffer + line
                else:
                    if len(buffer) < content_length:
                        # 这个检查算是个防御吧，实际应该用不到了。先保留，后续再看。
                        match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                        if match is not None:
                            start = match.start()
                            msg = buffer[0:start]
                            buffer = buffer[start:]
                            content_length = None
                            self.emit_message(msg.decode("utf-8"))
                        else:
                            line = self.process.stdout.readline(content_length - len(buffer))
                            buffer = buffer + line
                    else:
                        msg = buffer[0: content_length]
                        buffer = buffer[content_length:]
                        content_length = None
                        self.emit_message(msg.decode("utf-8"))
            except:
                logger.error(traceback.format_exc())
        logger.info("\n--- Lsp server exited, exit code: {}".format(self.process.returncode))
        logger.info(self.process.stdout.read())
        if self.process.stderr:
            logger.info(self.process.stderr.read())


class LspServer:

    def __init__(self, message_queue, project_path, server_info, server_name):
        # Init.
        self.message_queue = message_queue
        self.project_path = project_path
        self.server_info = server_info
        self.initialize_id = generate_request_id()
        self.server_name = server_name
        self.request_dict = {}
        self.root_path = self.project_path

        # Load library directories
        self.library_directories = [*map(os.path.expanduser, server_info.get("libraryDirectories", []))]

        # LSP server information.
        self.completion_trigger_characters = list()

        # Start LSP server.
        try:
            self.p = subprocess.Popen(self.server_info["command"], bufsize=DEFAULT_BUFFER_SIZE, stdin=PIPE, stdout=PIPE,
                                      stderr=stderr)
        except FileNotFoundError:
            message_emacs("ERROR: start LSP server {} failed, can't find file '{}'".format(
                self.server_info["name"], self.server_info["command"][0])
            )

            return

        # Notify user server is start.
        message_emacs("Start LSP server ({}) for {}...".format(self.server_info["name"], self.root_path))

        # Two separate thread (read/write) to communicate with LSP server.
        self.receiver = LspServerReceiver(self.p)
        self.receiver.start()

        self.sender = LspServerSender(self.p)
        self.sender.start()

        # All LSP server response running in ls_message_thread.
        self.ls_message_thread = threading.Thread(target=self.lsp_message_dispatcher)
        self.ls_message_thread.start()

        self.files: Dict[str, "FileAction"] = dict()

    def attach(self, fa: "FileAction"):
        file_key = path_as_key(fa.filepath)
        if file_key in self.files:
            logger.error(f"File {fa.filepath} opened again before close.")
            return

        self.files[file_key] = fa

        if len(self.files) == 1:
            # STEP 1: Say hello to LSP server.
            # Send 'initialize' request.
            self.send_initialize_request()

        # STEP 4: Tell LSP server open file.
        # We need send 'textDocument/didOpen' notification,
        # then LSP server will return file information, such as completion, find-define, find-references and rename etc.
        self.send_did_open_notification(fa.filepath, fa.external_file_link)

    def lsp_message_dispatcher(self):
        while True:
            message = self.receiver.get_message()
            try:
                self.handle_recv_message(message["content"])
            except:
                logger.error(traceback.format_exc())

    def send_initialize_request(self):
        self.sender.send_request("initialize", {
            "processId": os.getpid(),
            "rootPath": self.root_path,
            "clientInfo": {
                "name": "emacs",
                "version": get_emacs_version()
            },
            "rootUri": path_to_uri(self.project_path),
            "capabilities": self.server_info.get("capabilities", {}),
            "initializationOptions": self.server_info.get("initializationOptions", {})
        }, self.initialize_id, init=True)

    def parse_document_uri(self, filepath, external_file_link):
        """If FileAction include external_file_link return by LSP server, such as jdt.
        We should use external_file_link, such as uri 'jdt://xxx', otherwise use filepath as textDocument uri."""
        # Init with filepath.
        uri = path_to_uri(filepath)

        if external_file_link is not None:
            if urlparse(external_file_link).scheme != "":
                # Use external_file_link if we found scheme.
                uri = external_file_link
            else:
                # Otherwise external_file_link is filepath.
                uri = path_to_uri(external_file_link)

        return uri

    def send_did_open_notification(self, filepath, external_file_link=None):
        with open(filepath, encoding="utf-8") as f:
            self.sender.send_notification("textDocument/didOpen", {
                "textDocument": {
                    "uri": self.parse_document_uri(filepath, external_file_link),
                    "languageId": self.server_info["languageId"],
                    "version": 0,
                    "text": f.read()
                }
            })

    def send_did_close_notification(self, filepath):
        self.sender.send_notification("textDocument/didClose", {
            "textDocument": {
                "uri": path_to_uri(filepath),
            }
        })

    def send_did_save_notification(self, filepath):
        self.sender.send_notification("textDocument/didSave", {
            "textDocument": {
                "uri": path_to_uri(filepath)
            }
        })

    def send_did_change_notification(self, filepath, version, start, end, range_length, text):
        # STEP 5: Tell LSP server file content is changed.
        # This step is very IMPORTANT, make sure LSP server contain same content as client,
        # otherwise LSP server won't response client request, such as completion, find-define, find-references and rename etc.
        self.sender.send_notification("textDocument/didChange", {
            "textDocument": {
                "uri": path_to_uri(filepath),
                "version": version
            },
            "contentChanges": [
                {
                    "range": {
                        "start": start,
                        "end": end
                    },
                    "rangeLength": range_length,
                    "text": text
                }
            ]
        })

    def record_request_id(self, request_id, method, filepath, name):
        self.request_dict[request_id] = {
            "method": method,
            "filepath": filepath,
            "name": name
        }

    def send_shutdown_request(self):
        self.sender.send_request("shutdown", {}, generate_request_id())

    def send_exit_notification(self):
        self.sender.send_notification("exit", {})

    def get_server_workspace_change_configuration(self):
        return {
            "settings": self.server_info["settings"]
        }

    def handle_workspace_configuration_request(self, name, request_id, params):
        self.sender.send_response(request_id, [])

    def handle_recv_message(self, message: dict):
        if "error" in message:
            logger.error("\n--- Recv message (error):")
            logger.error(json.dumps(message, indent=3))
            return

        if "id" in message:
            if "method" in message:
                # server request
                logger.info("\n--- Recv request ({}): {}".format(message["id"], message["method"]))
            else:
                # server response
                if message["id"] in self.request_dict:
                    method = self.request_dict[message["id"]]["method"]
                    logger.info("\n--- Recv response ({}): {}".format(message["id"], method))
                else:
                    logger.info("\n--- Recv response ({})".format(message["id"]))
        else:
            if "method" in message:
                # server notification
                logger.info("\n--- Recv notification: %s", message["method"])
            else:
                # others
                logger.info("\n--- Recv message")

        if not ("method" in message and message["method"] in ["textDocument/publishDiagnostics"]):
            logger.debug(json.dumps(message, indent=3))

        if "id" in message:
            if message["id"] == self.initialize_id:
                # STEP 2: tell LSP server that client is ready.
                # We need wait LSP server response 'initialize', then we send 'initialized' notification.
                try:
                    # We pick up completion trigger characters from server.
                    # But some LSP server haven't this value, such as html/css LSP server.
                    self.completion_trigger_characters = message["result"]["capabilities"]["completionProvider"][
                        "triggerCharacters"]
                except KeyError:
                    pass

                self.sender.send_notification("initialized", {}, init=True)

                # STEP 3: Configure LSP server parameters.
                # After 'initialized' message finish, we should send 'workspace/didChangeConfiguration' notification.
                # The setting parameters of each language server are different.
                self.sender.send_notification("workspace/didChangeConfiguration",
                                              self.get_server_workspace_change_configuration(), init=True)

                self.sender.initialized.set()

                # Notify user server is ready.
                message_emacs("Start LSP server ({}) for {} complete, enjoy hacking!".format(
                        self.server_info["name"],
                        self.root_path
                ))
            else:
                if "method" not in message and message["id"] in self.request_dict:
                    self.message_queue.put({
                        "name": "server_response_message",
                        "content": (self.request_dict[message["id"]]["filepath"],
                                    self.request_dict[message["id"]]["name"],
                                    message["id"],
                                    message["result"])
                    })
                else:
                    if message["method"] == "workspace/configuration":
                        self.handle_workspace_configuration_request(message["method"], message["id"], message["params"])

    def close_file(self, filepath):
        # Send didClose notification when client close file.
        file_key = path_as_key(filepath)
        if file_key in self.files:
            self.send_did_close_notification(filepath)
            del self.files[file_key]

        # We need shutdown LSP server when last file closed, to save system memory.
        if len(self.files) == 0:
            self.send_shutdown_request()
            self.send_exit_notification()

            self.message_queue.put({
                "name": "server_process_exit",
                "content": self.server_name
            })

            # Don't need to wait LSP server response, kill immediately.
            if self.p is not None:
                os.kill(self.p.pid, 9)
