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

from sys import flags, stderr
from core.utils import *
from subprocess import PIPE
from threading import Thread
import io
import json
import os
import queue
import re
import subprocess
import threading
import traceback

DEFAULT_BUFFER_SIZE = 100000000  # we need make buffer size big enough, avoid pipe hang by big data response from LSP server


class JsonEncoder(json.JSONEncoder):

    def default(self, o):  # pylint: disable=E0202
        return o.__dict__


class LspBridgeListener(Thread):

    def __init__(self, process, lsp_message_queue):
        Thread.__init__(self)

        self.process = process
        self.reader = io.BufferedReader(io.FileIO(self.process.stdout.fileno()), buffer_size=DEFAULT_BUFFER_SIZE)
        self.lsp_message_queue = lsp_message_queue

    def emit_message(self, line):
        if line != "":
            try:
                # Send message.
                self.lsp_message_queue.put({
                    "name": "lsp_recv_message",
                    "content": json.loads(line)
                })

            except:
                traceback.print_exc()

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
                traceback.print_exc()
        logger.info("\n--- Lsp server exited, exit code: {}".format(self.process.returncode))
        logger.info(self.process.stdout.read())
        if self.process.stderr:
            logger.info(self.process.stderr.read())


class SendRequest(Thread):

    def __init__(self, process, name, params, id):
        Thread.__init__(self)

        self.process = process
        self.name = name
        self.params = params
        self.id = id

    def run(self):
        if self.process.returncode is not None:
            return

        message_dict = {}
        message_dict["jsonrpc"] = "2.0"
        message_dict["method"] = self.name
        message_dict["params"] = self.params
        message_dict["id"] = self.id

        json_string = json.dumps(message_dict, cls=JsonEncoder, separators=(',', ':'))
        json_message = "Content-Length: {}\r\n\r\n{}".format(len(json_string), json_string)

        self.process.stdin.write(json_message.encode("utf-8"))
        self.process.stdin.flush()

        logger.info("\n--- Send request ({}): {}".format(self.id, self.name))

        logger.debug(json.dumps(message_dict, indent=3))


class SendNotification(Thread):

    def __init__(self, process, lsp_message_queue, name, params):
        Thread.__init__(self)

        self.process = process
        self.lsp_message_queue = lsp_message_queue
        self.name = name
        self.params = params

    def run(self):
        if self.process.returncode is not None:
            return

        message_dict = {}
        message_dict["jsonrpc"] = "2.0"
        message_dict["method"] = self.name
        message_dict["params"] = self.params

        json_string = json.dumps(message_dict, cls=JsonEncoder, separators=(',', ':'))
        json_message = "Content-Length: {}\r\n\r\n{}".format(len(json_string), json_string)

        self.process.stdin.write(json_message.encode("utf-8"))
        self.process.stdin.flush()

        self.lsp_message_queue.put({
            "name": "lsp_send_notification",
            "content": (self.name, self.params)
        })

        logger.info("\n--- Send notification: {}".format(self.name))

        logger.debug(json.dumps(message_dict, indent=3))


class SendResponse(Thread):

    def __init__(self, process, name, id, result):
        Thread.__init__(self)

        self.process = process
        self.name = name
        self.id = id
        self.result = result

    def run(self):
        if self.process.returncode is not None:
            return

        message_dict = {}
        message_dict["jsonrpc"] = "2.0"
        message_dict["id"] = self.id
        message_dict["result"] = self.result

        json_string = json.dumps(message_dict, cls=JsonEncoder, separators=(',', ':'))
        json_message = "Content-Length: {}\r\n\r\n{}".format(len(json_string), json_string)

        self.process.stdin.write(json_message.encode("utf-8"))
        self.process.stdin.flush()

        logger.info("\n--- Send response ({}): {}".format(self.id, self.name))

        logger.debug(json.dumps(message_dict, indent=3))


class LspServer(object):

    def __init__(self, message_queue, file_action):
        object.__init__(self)

        # Init.
        self.message_queue = message_queue
        self.project_path = file_action.project_path
        self.server_type = file_action.lang_server_info["name"]
        self.server_info = file_action.lang_server_info
        self.first_file_path = file_action.filepath
        self.initialize_id = file_action.initialize_id
        self.server_name = file_action.get_lsp_server_name()
        self.shutdown_id = -1
        self.sender_threads = []
        self.request_dict = {}
        self.open_file_dict = {} # contain file opened in current project
        self.filepath_uri_dict = {} # save the mapping of filepath and uri
        self.root_path = self.project_path

        # All LSP server response running in ls_message_thread.
        self.lsp_message_queue = queue.Queue()
        self.ls_message_thread = threading.Thread(target=self.lsp_message_dispatcher)
        self.ls_message_thread.start()

        # Load library directories
        self.library_directories = [*map(os.path.expanduser, file_action.lang_server_info.get("libraryDirectories", []))]
        self.message_queue.put({
            "name": "update_lang_library_directories",
            "content": {
                "lang": self.server_type,
                "dires": self.library_directories
            }
        })
        for lib_dir in self.library_directories:
            self.message_queue.put({
                "name": "update_library_to_server_dict",
                "content": {
                    "library": "{}#{}".format(lib_dir, self.server_type),
                    "lsp_server_name": self.server_name
                }
            })

        # LSP server information.
        self.completion_trigger_characters = list()

        # Start LSP server process.
        self.start_lsp_server_process()

    def start_lsp_server_process(self):
        # Start LSP sever.
        try:
            self.p = subprocess.Popen(self.server_info["command"], bufsize=DEFAULT_BUFFER_SIZE, stdin=PIPE, stdout=PIPE,
                                      stderr=stderr)
        except FileNotFoundError:
            eval_in_emacs("message", ["LSP-Bridge] ERROR: start LSP server {} failed, not found file '{}'".format(
                self.server_info["name"], self.server_info["command"][0]
            )])

            return

        # Notify user server is start.
        eval_in_emacs("message",
                      ["[LSP-Bridge] Start LSP server ({}) for {}...".format(self.server_info["name"], self.root_path)])

        # A separate thread is used to read the message returned by the LSP server.
        self.listener_thread = LspBridgeListener(self.p, self.lsp_message_queue)
        self.listener_thread.start()

        # STEP 1: Say hello to LSP server.
        # Send 'initialize' request.
        self.send_initialize_request()

    def lsp_server_is_started(self):
        return getattr(self, "p", None) is None

    def lsp_message_dispatcher(self):
        while True:
            message = self.lsp_message_queue.get(True)
            try:
                if message["name"] == "lsp_recv_message":
                    self.handle_recv_message(message["content"])
                elif message["name"] == "lsp_send_notification":
                    self.handle_send_notification(*message["content"])

                self.lsp_message_queue.task_done()
            except:
                traceback.print_exc()

    def send_initialize_request(self):
        self.send_to_request("initialize", {
            "processId": os.getpid(),
            "rootPath": self.root_path,
            "clientInfo": {
                "name": "emacs",
                "version": get_emacs_version()
            },
            "rootUri": path_to_uri(self.project_path),
            "capabilities": self.server_info.get("capabilities", {}),
            "initializationOptions": self.server_info.get("initializationOptions", {})
        }, self.initialize_id)

    def send_did_open_notification(self, filepath, lsp_buffer_uri_or_path):
        filekey = path_as_key(filepath)
        if filekey not in self.open_file_dict:
            self.open_file_dict[filekey] = ""

            if lsp_buffer_uri_or_path.startswith("/"):
                self.filepath_uri_dict[filepath] = path_to_uri(lsp_buffer_uri_or_path)
            else:
                self.filepath_uri_dict[filepath] = lsp_buffer_uri_or_path
            with open(filepath, encoding="utf-8") as f:
                self.send_to_notification("textDocument/didOpen",
                                          {
                                              "textDocument": {
                                                  "uri": self.filepath_uri_dict[filepath],
                                                  "languageId": self.server_info["languageId"],
                                                  "version": 0,
                                                  "text": f.read()
                                              }
                                          })
        else:
            logger.info("File {} has opened in server {}".format(filepath, self.server_name))

    def send_did_close_notification(self, filepath):
        self.send_to_notification("textDocument/didClose",
                                  {
                                      "textDocument": {
                                          "uri": path_to_uri(filepath),
                                      }
                                  })

    def send_did_save_notification(self, filepath):
        self.send_to_notification("textDocument/didSave",
                                  {
                                      "textDocument": {
                                          "uri": path_to_uri(filepath)
                                      }
                                  })

    def send_did_change_notification(self, filepath, version, start, end, range_length, text):
        # STEP 5: Tell LSP server file content is changed.
        # This step is very IMPORTANT, make sure LSP server contain same content as client,
        # otherwise LSP server won't response client request, such as completion, find-define, find-references and rename etc.
        self.send_to_notification("textDocument/didChange",
                                  {
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

    def record_request_id(self, request_id, method, filepath, type):
        self.request_dict[request_id] = {
            "method": method,
            "filepath": filepath,
            "type": type
        }

    def send_shutdown_request(self):
        self.shutdown_id = generate_request_id()
        self.send_to_request("shutdown", {}, self.shutdown_id)

    def send_exit_notification(self):
        self.send_to_notification("exit", {})

    def get_server_workspace_change_configuration(self):
        return {
            "settings": self.server_info["settings"]
        }

    def handle_workspace_configuration_request(self, name, id, params):
        self.send_to_response(name, id, {})

    def handle_recv_message(self, message):
        if "error" in message.keys():
            logger.error("\n--- Recv message (error):")
            logger.error(json.dumps(message, indent=3))
            return

        if "id" in message.keys():
            if "method" in message.keys():
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
            if "method" in message.keys():
                # server notification
                logger.info("\n--- Recv notification: %s", message["method"])
            else:
                # others
                logger.info("\n--- Recv message")

        if "method" in message and message["method"] not in ["textDocument/publishDiagnostics"]:
            logger.debug(json.dumps(message, indent=3))

        if "id" in message.keys():
            if message["id"] == self.initialize_id:
                # STEP 2: tell LSP server that client is ready.
                # We need wait LSP server response 'initialize', then we send 'initialized' notification.
                try:
                    # We pick up completion trigger characters from server.
                    # But some LSP server haven't this value, such as html/css LSP server.
                    self.completion_trigger_characters = message["result"]["capabilities"]["completionProvider"][
                        "triggerCharacters"]
                except:
                    pass

                self.send_to_notification("initialized", {})
            else:
                if "method" not in message.keys() and message["id"] in self.request_dict:
                    self.message_queue.put({
                        "name": "server_response_message",
                        "content": (self.request_dict[message["id"]]["filepath"],
                                    self.request_dict[message["id"]]["type"],
                                    message["id"],
                                    message["result"])
                    })
                else:
                    if message["method"] == "workspace/configuration":
                        self.handle_workspace_configuration_request(message["method"], message["id"], message["params"])

    def handle_send_notification(self, name, params):
        if name == "initialized":
            # STEP 3: Configure LSP server parameters.
            # After 'initialized' message finish, we should send 'workspace/didChangeConfiguration' notification.
            # The setting parameters of each language server are different.
            self.send_to_notification("workspace/didChangeConfiguration",
                                      self.get_server_workspace_change_configuration())

            # STEP 4: Tell LSP server open file.
            # We need send 'textDocument/didOpen' notification,
            # then LSP server will return file information, such as completion, find-define, find-references and rename etc.
            self.send_did_open_notification(self.first_file_path, self.first_file_path)

            # Notify user server is ready.
            eval_in_emacs("message", [
                "[LSP-Bridge] Start LSP server ({}) for {} complete, enjoy hacking!".format(self.server_info["name"],
                                                                                            self.root_path)])
        elif name == "textDocument/didOpen":
            fileuri = params["textDocument"]["uri"]
            if fileuri.startswith("file://"):
                fileuri = uri_to_path(fileuri)

            self.message_queue.put({
                "name": "server_file_opened",
                "content": fileuri
            })

    def send_to_request(self, name, params, request_id):
        # Request message must be contain unique request id.
        if self.lsp_server_is_started():
            self.start_lsp_server_process()
        else:
            sender_thread = SendRequest(self.p, name, params, request_id)
            self.sender_threads.append(sender_thread)
            sender_thread.start()

    def send_to_notification(self, name, params):
        if self.lsp_server_is_started():
            self.start_lsp_server_process()
        else:
            sender_thread = SendNotification(self.p, self.lsp_message_queue, name, params)
            self.sender_threads.append(sender_thread)
            sender_thread.start()

    def send_to_response(self, name, id, result):
        if self.lsp_server_is_started():
            self.start_lsp_server_process()
        else:
            sender_thread = SendResponse(self.p, name, id, result)
            self.sender_threads.append(sender_thread)
            sender_thread.start()

    def close_file(self, filepath):
        # Send didClose notification when client close file.
        filekey = path_as_key(filepath)
        if filekey in self.open_file_dict:
            self.send_did_close_notification(filepath)
            del self.open_file_dict[filekey]

        # We need shutdown LSP server when last file closed, to save system memory.
        if len(self.open_file_dict) == 0:
            self.send_shutdown_request()
            self.send_exit_notification()

            self.message_queue.put({
                "name": "server_process_exit",
                "content": self.server_name
            })

            # Don't need wait LSP server response, kill immediately.
            if self.lsp_server_is_started():
                os.kill(self.p.pid, 9)
