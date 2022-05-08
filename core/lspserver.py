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

from core.utils import generate_request_id, get_emacs_var, eval_in_emacs
from subprocess import PIPE
from threading import Thread
import json
import os
import queue
import re
import subprocess
import threading
import traceback

LSP_TYPE_LANGUAGE_TYPE_DICT = {
    "pyright": "python",
    "solargraph": "ruby"
}

class JsonEncoder(json.JSONEncoder):

    def default(self, o): # pylint: disable=E0202
        return o.__dict__

class LspBridgeListener(Thread):

    def __init__(self, process, lsp_message_queue):
        Thread.__init__(self)

        self.process = process
        self.lsp_message_queue = lsp_message_queue

    def is_end_number(self, string):
        text = re.compile(r".*[0-9]$")
        if text.match(string):
            return True
        else:
            return False

    def emit_message(self, message):
        try:
            # If message is not end with char '}', need remove unnecessary string (such as "Content" ),
            # to make json load message correctly.
            if not message.endswith("}"):
                quote_index = message.rfind("}")
                message = message[:quote_index + 1]

            self.lsp_message_queue.put({
                "name": "lsp_recv_message",
                "content": json.loads(message)
            })
        except:
            traceback.print_exc()

    def run(self):
        while self.process.poll() is None:
            try:
                line = self.process.stdout.readline().strip()

                # print("#### ", line)

                if line.startswith("{"):
                    self.emit_message(line)
                elif self.is_end_number(line) and not line.startswith("{"):
                    # LSP message need read 3 times.
                    # 1. Read Content-Length
                    # 2. Drop empty line
                    # 3. Read message base on Content-Length.

                    # Sometimes, Content-Length header is not starts with `Content-Length',
                    # if line not starts with char '{', we need parse number at end of line.

                    # Read Content-Length.
                    splits = line.split(":")
                    length = int(splits[1].strip())

                    # Drop empty line.
                    self.process.stdout.readline()

                    # Emit message.
                    message = self.process.stdout.readline(length).strip()

                    if message != "":
                        self.emit_message(message)
            except:
                traceback.print_exc()

class SendRequest(Thread):

    def __init__(self, process, name, params, id):
        Thread.__init__(self)
        
        self.process = process
        self.name = name
        self.params = params
        self.id = id

    def run(self):
        message_dict = {}
        message_dict["jsonrpc"] = "2.0"
        message_dict["method"] = self.name
        message_dict["params"] = self.params
        message_dict["id"] = self.id

        json_string = json.dumps(message_dict, cls=JsonEncoder, separators=(',', ':'))
        json_message = "Content-Length: {}\r\n\r\n{}\n".format(len(json_string) + 1, json_string)

        self.process.stdin.write(json_message)
        self.process.stdin.flush()

        print("\n--- Send request (1): {}".format(self.name, self.id))

        print(json.dumps(message_dict, indent = 3))

class SendNotification(Thread):

    def __init__(self, process, lsp_message_queue, name, params):
        Thread.__init__(self)

        self.process = process
        self.lsp_message_queue = lsp_message_queue
        self.name = name
        self.params = params

    def run(self):
        message_dict = {}
        message_dict["jsonrpc"] = "2.0"
        message_dict["method"] = self.name
        message_dict["params"] = self.params

        json_string = json.dumps(message_dict, cls=JsonEncoder, separators=(',', ':'))
        json_message = "Content-Length: {}\r\n\r\n{}\n".format(len(json_string) + 1, json_string)

        self.process.stdin.write(json_message)
        self.process.stdin.flush()

        self.lsp_message_queue.put({
            "name": "lsp_send_notification",
            "content": (self.name, self.params)
        })
        
        print("\n--- Send notification: {}".format(self.name))

        print(json.dumps(message_dict, indent = 3))

class SendResponse(Thread):

    def __init__(self, process, name, id, result):
        Thread.__init__(self)

        self.process = process
        self.name = name
        self.id = id
        self.result = result

    def run(self):
        message_dict = {}
        message_dict["jsonrpc"] = "2.0"
        message_dict["id"] = self.id
        message_dict["result"] = self.result

        json_string = json.dumps(message_dict, cls=JsonEncoder, separators=(',', ':'))
        json_message = "Content-Length: {}\r\n\r\n{}\n".format(len(json_string) + 1, json_string)

        self.process.stdin.write(json_message)
        self.process.stdin.flush()

        print("\n--- Send response: {}".format(self.name))

        print(json.dumps(message_dict, indent = 3))

class LspServer(object):

    def __init__(self, message_queue, file_action):
        object.__init__(self)
        
        # Init.
        self.message_queue = message_queue
        
        self.project_path = file_action.project_path
        self.server_type = file_action.lsp_server_type
        self.first_file_path = file_action.filepath
        self.initialize_id = file_action.initialize_id
        self.server_name = file_action.get_lsp_server_name()
        self.shutdown_id = -1
        self.sender_threads = []
        self.request_dict = {}
        self.open_file_dict = {} # contain file opened in current project
        self.rootPath = self.project_path

        # All LSP server response running in ls_message_thread.
        self.lsp_message_queue = queue.Queue()
        self.ls_message_thread = threading.Thread(target=self.lsp_message_dispatcher)
        self.ls_message_thread.start()

        # Start LSP sever.
        self.p = subprocess.Popen(self.get_server_command(),
                                  bufsize=100000000, # we need make buffer size big enough, avoid pipe hang by big data response from LSP server 
                                  text=True,
                                  stdin=PIPE, stdout=PIPE, stderr=PIPE)

        # A separate thread is used to read the message returned by the LSP server.
        self.listener_thread = LspBridgeListener(self.p, self.lsp_message_queue)
        self.listener_thread.start()

        # STEP 1: Say hello to LSP server.
        # Send 'initialize' request.
        self.send_initialize_request()
        
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
        initialize_options = {
            "processId": os.getpid(),
            "rootPath": self.rootPath,
            "clientInfo": {
                "name": "emacs",
                "version": "GNU Emacs 28.1 (build 1, x86_64-pc-linux-gnu, GTK+ Version 3.24.33, cairo version 1.17.6)\n of 2022-04-04"
            },
            "rootUri": "file://" + self.project_path,
            "capabilities": {},
            "initializationOptions": {}
        }
        self.send_to_request("initialize", initialize_options, self.initialize_id)

    def send_did_open_notification(self, filepath):
        if filepath not in self.open_file_dict:
            self.open_file_dict[filepath] = ""

            with open(filepath) as f:
                self.send_to_notification("textDocument/didOpen",
                                          {
                                              "textDocument": {
                                                  "uri": "file://" + filepath,
                                                  "languageId": LSP_TYPE_LANGUAGE_TYPE_DICT[self.server_type],
                                                  "version": 0,
                                                  "text": f.read()
                                              }
                                          })
        else:
            print("File {} has opened in server {}".format(filepath, self.server_name))

    def send_did_close_notification(self, filepath):
        self.send_to_notification("textDocument/didClose",
                                  {
                                      "textDocument": {
                                          "uri": "file://" + filepath,
                                      }
                                  })


    def send_did_change_notification(self, filepath, version, start_row, start_character, end_row, end_character, range_length, text):
        # STEP 5: Tell LSP server file content is changed.
        # This step is very IMPORTANT, make sure LSP server contain same content as client,
        # otherwise LSP server won't response client request, such as completion, find-define, find-references and rename etc.
        self.send_to_notification("textDocument/didChange",
                                  {
                                      "textDocument": {
                                          "uri": "file://" + filepath,
                                          "version": version
                                      },
                                      "contentChanges": [
                                          {
                                              "range": {
                                                  "start": {
                                                      "line": start_row - 1,
                                                      "character": start_character
                                                  },
                                                  "end": {
                                                      "line": end_row - 1,
                                                      "character": end_character
                                                  }
                                              },
                                              "rangeLength": range_length,
                                              "text": text
                                          }
                                      ]
                                  })

    def record_request_id(self, request_id, filepath, type):
        self.request_dict[request_id] = {
            "filepath": filepath,
            "type": type
        }

    def send_completion_request(self, request_id, filepath, type, row, column, char):
        self.record_request_id(request_id, filepath, type)

        # STEP 6: Calculate completion candidates for current point.
        if char == ".":
            self.send_to_request("textDocument/completion",
                                 {
                                     "textDocument": {
                                         "uri": "file://" + filepath
                                     },
                                     "position": {
                                         "line": row - 1,
                                         "character": column
                                     },
                                     "context": {
                                         "triggerKind": 2,
                                         "triggerCharacter": char
                                     }
                                 },
                                 request_id)
        else:
            self.send_to_request("textDocument/completion",
                                 {
                                     "textDocument": {
                                         "uri": "file://" + filepath
                                     },
                                     "position": {
                                         "line": row - 1,
                                         "character": column
                                     },
                                     "context": {
                                         "triggerKind": 3
                                     }
                                 },
                                 request_id)

    def send_find_define_request(self, request_id, filepath, type, row, column):
        self.record_request_id(request_id, filepath, type)

        self.send_to_request("textDocument/declaration",
                             {
                                 "textDocument": {
                                     "uri": "file://" + filepath
                                 },
                                 "position": {
                                     "line": row - 1,
                                     "character": column
                                 }
                             },
                             request_id)

    def send_find_references_request(self, request_id, filepath, type, row, column):
        self.record_request_id(request_id, filepath, type)

        self.send_to_request("textDocument/references",
                             {
                                 "textDocument": {
                                     "uri": "file://" + filepath
                                 },
                                 "position": {
                                     "line": row - 1,
                                     "character": column
                                 },
                                 "context": {
                                     "includeDeclaration": False
                                 }
                             },
                             request_id)

    def send_prepare_rename_request(self, request_id, filepath, type, row, column):
        self.record_request_id(request_id, filepath, type)

        self.send_to_request("textDocument/prepareRename",
                             {
                                 "textDocument": {
                                     "uri": "file://" + filepath
                                 },
                                 "position": {
                                     "line": row - 1,
                                     "character": column
                                 }
                             },
                             request_id)

    def send_rename_request(self, request_id, filepath, type, row, column, new_name):
        self.record_request_id(request_id, filepath, type)

        self.send_to_request("textDocument/rename",
                             {
                                 "textDocument": {
                                     "uri": "file://" + filepath
                                 },
                                 "position": {
                                     "line": row - 1,
                                     "character": column
                                 },
                                 "newName": new_name
                             },
                             request_id)

    def send_shutdown_request(self):
        self.shutdown_id = generate_request_id()
        self.send_to_request("shutdown", {}, self.shutdown_id)

    def send_exit_notification(self):
        self.send_to_notification("exit", {})

    def get_server_command(self):
        if self.server_type == "pyright":
            return ["pyright-langserver", "--stdio"]
        elif self.server_type == "solargraph":
            return ["solargraph", "stdio"]

    def get_server_workspace_change_configuration(self):
        if self.server_type == "pyright":
            eval_in_emacs("lsp-bridge-set-current-python-command", [])
            return {"settings": {
                "analysis": {
                    "autoImportCompletions": True,
                    "typeshedPaths": [],
                    "stubPath": "",
                    "useLibraryCodeForTypes": True,
                    "diagnosticMode": "openFilesOnly",
                    "typeCheckingMode": "basic",
                    "logLevel": "verbose",
                    "autoSearchPaths": True,
                    "extraPaths": []
                },
                "pythonPath": get_emacs_var("lsp-bridge-current-python-command"),
                "venvPath": ""
            }}
        elif self.server_type == "solargraph":
            {"settings": {
                "solargraph": {
                    "logLevel": "warn",
                    "folding": True,
                    "references": True,
                    "rename": True,
                    "definitions": True,
                    "symbols": True,
                    "formatting": True,
                    "autoformat": True,
                    "diagnostics": True,
                    "hover": True,
                    "completion": True,
                    "useBundler": False
                }
            }}

    def handle_recv_message(self, message):
        method_name = ""
        if "method" in message.keys():
            method_name = message["method"]
            print("\n--- Recv message: ", method_name)
        else:
            print("\n--- Recv message")

        print(json.dumps(message, indent = 3))

        if "id" in message.keys():
            if message["id"] == self.initialize_id:
                # STEP 2: tell LSP server that client is ready.
                # We need wait LSP server response 'initialize', then we send 'initialized' notification.
                self.send_to_notification("initialized", {})
            else:
                if message["id"] in self.request_dict and not "error" in message:
                    self.message_queue.put({
                        "name": "server_response_message",
                        "content": (self.request_dict[message["id"]]["filepath"],
                                    self.request_dict[message["id"]]["type"],
                                    message["id"],
                                    message["result"])
                    })

    def handle_send_notification(self, name, params):
        if name == "initialized":
            # STEP 3: Configure LSP server parameters.
            # After 'initialized' message finish, we should send 'workspace/didChangeConfiguration' notification.
            # The setting parameters of each language server are different.
            self.send_to_notification("workspace/didChangeConfiguration", self.get_server_workspace_change_configuration())
            
            # STEP 4: Tell LSP server open file.
            # We need send 'textDocument/didOpen' notification, 
            # then LSP server will return file information, such as completion, find-define, find-references and rename etc.
            self.send_did_open_notification(self.first_file_path)
        elif name == "textDocument/didOpen":
            fileuri = params["textDocument"]["uri"]
            if fileuri.startswith("file://"):
                fileuri = fileuri[len("file://"):]

            self.message_queue.put({
                "name": "server_file_opened",
                "content": fileuri
            })

    def send_to_request(self, name, params, request_id):
        # Request message must be contain unique request id.
        sender_thread = SendRequest(self.p, name, params, request_id)
        self.sender_threads.append(sender_thread)
        sender_thread.start()

    def send_to_notification(self, name, params):
        sender_thread = SendNotification(self.p, self.lsp_message_queue, name, params)
        self.sender_threads.append(sender_thread)
        sender_thread.start()

    def send_to_response(self, name, id, result):
        sender_thread = SendResponse(self.p, name, id, result)
        self.sender_threads.append(sender_thread)
        sender_thread.start()

    def close_file(self, filepath):
        # Send didClose notification when client close file.
        if filepath in self.open_file_dict:
            self.send_did_close_notification(filepath)
            del self.open_file_dict[filepath]

        # We need shutdown LSP server when last file closed, to save system memory.
        if len(self.open_file_dict.keys()) == 0:
            self.send_shutdown_request()
            self.send_exit_notification()

            self.message_queue.put({
                "name": "server_process_exit",
                "content": self.server_name
            })
        
            # Don't need wait LSP server response, kill immediately.
            os.kill(self.p.pid, 9)
