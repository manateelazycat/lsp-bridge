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
import subprocess
import json
import re
from subprocess import PIPE
from PyQt6.QtCore import QObject, QThread, QTimer
from PyQt6 import QtCore

class JsonEncoder(json.JSONEncoder):

    def default(self, o): # pylint: disable=E0202
        return o.__dict__

class LspBridgeListener(QThread):

    recv_message = QtCore.pyqtSignal(dict)

    def __init__(self, process):
        QThread.__init__(self)

        self.process = process

    def is_end_number(self, string):
        text = re.compile(r".*[0-9]$")
        if text.match(string):
            return True
        else:
            return False        
        
    def run(self):
        while self.process.poll() is None:
            line = self.process.stdout.readline().strip()

            # LSP message need read 3 times.
            # 1. Read Content-Length
            # 2. Drop empty line
            # 3. Read message base on Content-Length.
            
            # Sometimes, Content-Length header is not starts with `Content-Length', 
            # if line not starts with char '{', we need parse number at end of line.
            if self.is_end_number(line) and not line.startswith("{"):
                # Read Content-Length.
                splits = line.split(":")
                length = int(splits[1].strip())

                # Drop empty line.
                self.process.stdout.readline()

                # Emit message.
                message = self.process.stdout.readline(length).strip()
                
                if message != "":
                    try:
                        # If message is not end with char '}', need remove unnecessary string (such as "Content" ), 
                        # to make json load message correctly.
                        if not message.endswith("}"):
                            quote_index = message.rfind("}")
                            message = message[:quote_index + 1]
                        
                        self.recv_message.emit(json.loads(message))
                    except:
                        print("* Parse server message failed.")
                        import traceback
                        traceback.print_exc()

class SendRequest(QThread):

    send_message = QtCore.pyqtSignal(str)

    def __init__(self, process, name, params, id):
        QThread.__init__(self)

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

        self.send_message.emit(self.name)

        print("\n--- Send request (1): {}".format(self.name, self.id))

        # print(json.dumps(message_dict, indent = 3))

class SendNotification(QThread):

    send_notification = QtCore.pyqtSignal(str)

    def __init__(self, process, name, params):
        QThread.__init__(self)

        self.process = process
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

        self.send_notification.emit(self.name)

        print("\n--- Send notification: {}".format(self.name))

        # print(json.dumps(message_dict, indent = 3))

class SendResponse(QThread):

    send_response = QtCore.pyqtSignal(str)

    def __init__(self, process, name, id, result):
        QThread.__init__(self)

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

        self.send_response.emit(self.name)

        print("\n--- Send response: {}".format(self.name))

        # print(json.dumps(message_dict, indent = 3))

class LspServer(QObject):

    def __init__(self, file_action):
        QObject.__init__(self)

        self.project_path = file_action.project_path
        self.server_type = file_action.lsp_server_type
        self.first_file_path = file_action.filepath
        self.initialize_id = file_action.initialize_id

        self.p = subprocess.Popen(self.get_server_command(), text=True,
                                  stdin=PIPE, stdout=PIPE, stderr=PIPE)

        self.listener_thread = LspBridgeListener(self.p)
        self.listener_thread.recv_message.connect(self.handle_server_message)
        self.listener_thread.start()

        self.sender_threads = []

        self.rootPath = self.project_path
        self.rootUri = "file://" + self.project_path

        self.send_initialize_request()

    def send_initialize_request(self):
        initialize_options = {
            "processId": os.getpid(),
            "rootPath": self.rootPath,
            "clientInfo": {
                "name": "emacs",
                "version": "GNU Emacs 28.1 (build 1, x86_64-pc-linux-gnu, GTK+ Version 3.24.33, cairo version 1.17.6)\n of 2022-04-04"
            },
            "rootUri": self.rootUri,
            "capabilities": {},
            "initializationOptions": {}
        }
        self.send_to_request("initialize", initialize_options, self.initialize_id)

    def send_did_open_notification(self, filepath):
        print("Send didOpen notification: ", filepath)

        with open(filepath) as f:
            self.send_to_notification("textDocument/didOpen",
                                      {
                                          "textDocument": {
                                              "uri": "file://" + filepath,
                                              "languageId": "python",
                                              "version": 0,
                                              "text": f.read()
                                          }
                                      })

    def send_completion_request(self, request_id, filepath, row, column, char):
        self.send_to_notification("textDocument/didChange",
                                  {
                                      "textDocument": {
                                          "uri": "file://" + filepath,
                                          "version": 1
                                      },
                                      "contentChanges": [
                                          {
                                              "range": {
                                                  "start": {
                                                      "line": row,
                                                      "character": column - 1
                                                  },
                                                  "end": {
                                                      "line": row,
                                                      "character": column - 1
                                                  }
                                              },
                                              "rangeLength": 0,
                                              "text": char
                                          }
                                      ]
                                  })

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
                                     "triggerKind": 1
                                 }
                             },
                             request_id)

    def get_server_command(self):
        if self.server_type == "pyright":
            return ["pyright-langserver", "--stdio"]

    def get_server_workspace_change_configuration(self):
        if self.server_type == "pyright":
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
                "pythonPath": "/usr/bin/python",
                "venvPath": ""
            }}

    def respond_initialize(self):
        self.send_to_notification("initialized", {})

    def handle_server_message(self, message):
        if "id" in message.keys():
            if message["id"] == self.initialize_id:
                self.respond_initialize()

        print("\n--- Recv message")
        print(json.dumps(message, indent = 3))

    def handle_send_notification(self, name):
        if name == "initialized":
            self.send_to_notification("workspace/didChangeConfiguration", self.get_server_workspace_change_configuration())

            self.send_did_open_notification(self.first_file_path)

    def send_to_request(self, name, params, request_id):
        sender_thread = SendRequest(self.p, name, params, request_id)
        self.sender_threads.append(sender_thread)
        sender_thread.start()

    def send_to_notification(self, name, params):
        sender_thread = SendNotification(self.p, name, params)
        sender_thread.send_notification.connect(self.handle_send_notification)
        self.sender_threads.append(sender_thread)
        sender_thread.start()

    def send_to_response(self, name, id, result):
        sender_thread = SendResponse(self.p, name, id, result)
        self.sender_threads.append(sender_thread)
        sender_thread.start()
