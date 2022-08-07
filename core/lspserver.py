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

import copy
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
from typing import Dict, TYPE_CHECKING
from core.mergedeep import merge

from core.handler import Handler

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
        json_content = json.dumps(message)
        
        message_str = "Content-Length: {}\r\n\r\n{}".format(len(json_content), json_content)

        self.process.stdin.write(message_str.encode("utf-8"))    # type: ignore
        self.process.stdin.flush()    # type: ignore

        logger.info("\n--- Send ({}): {}".format(
            message.get('id', 'notification'), message.get('method', 'response')))

        logger.debug(json.dumps(message, indent=3))

    def run(self) -> None:
        try:
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
        except:
            logger.error(traceback.format_exc())
            
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
                "content": parse_json_content(line)
            })
        except:
            logger.error(traceback.format_exc())

    def run(self):
        try:
            content_length = None
            buffer = bytearray()
            while self.process.poll() is None:
                if content_length is None:
                    match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                    if match is not None:
                        end = match.end()
                        parts = match.group(0).decode("utf-8").strip().split(": ")
                        content_length = int(parts[1])
            
                        buffer = buffer[end:]
                    else:
                        line = self.process.stdout.readline()    # type: ignore
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
                            line = self.process.stdout.readline(content_length - len(buffer))    # type: ignore
                            buffer = buffer + line
                    else:
                        msg = buffer[0: content_length]
                        buffer = buffer[content_length:]
                        content_length = None
                        self.emit_message(msg.decode("utf-8"))
            logger.info("\n--- Lsp server exited, exit code: {}".format(self.process.returncode))
            logger.info(self.process.stdout.read())    # type: ignore
            if self.process.stderr:
                logger.info(self.process.stderr.read())
        except:
            logger.error(traceback.format_exc())

class LspServer:
    def __init__(self, message_queue, project_path, server_info, server_name):
        self.message_queue = message_queue
        self.project_path = project_path
        self.server_info = server_info
        self.initialize_id = generate_request_id()
        self.server_name = server_name
        self.request_dict: Dict[int, Handler] = dict()
        self.root_path = self.project_path

        # LSP server information.
        self.completion_trigger_characters = list()
        self.completion_resolve_provider = False
        self.rename_prepare_provider = False
        self.code_action_provider = False
        self.code_format_provider = False
        self.signature_help_provider = False
        self.code_action_kinds = [
            "quickfix",
            "refactor",
            "refactor.extract",
            "refactor.inline",
            "refactor.rewrite",
            "source",
            "source.organizeImports"]

        # Start LSP server.
        self.lsp_subprocess = subprocess.Popen(self.server_info["command"], bufsize=DEFAULT_BUFFER_SIZE, stdin=PIPE, stdout=PIPE, stderr=stderr)

        # Notify user server is start.
        message_emacs("Start LSP server ({}) for {}...".format(self.server_info["name"], self.root_path))

        # Two separate thread (read/write) to communicate with LSP server.
        self.receiver = LspServerReceiver(self.lsp_subprocess)
        self.receiver.start()

        self.sender = LspServerSender(self.lsp_subprocess)
        self.sender.start()

        # All LSP server response running in ls_message_thread.
        self.ls_message_thread = threading.Thread(target=self.lsp_message_dispatcher)
        self.ls_message_thread.start()

        self.files: Dict[str, "FileAction"] = dict()

    def attach(self, fa: "FileAction"):
        if is_in_path_dict(self.files, fa.filepath):
            logger.error(f"File {fa.filepath} opened again before close.")
            return

        add_to_path_dict(self.files, fa.filepath, fa)

        if len(self.files) == 1:
            # STEP 1: Say hello to LSP server.
            # Send 'initialize' request.
            self.send_initialize_request()

        # STEP 4: Tell LSP server open file.
        # We need send 'textDocument/didOpen' notification,
        # then LSP server will return file information, such as completion, find-define, find-references and rename etc.
        self.send_did_open_notification(fa.filepath, fa.external_file_link)

    def lsp_message_dispatcher(self):
        try:
            while True:
                message = self.receiver.get_message()
                self.handle_recv_message(message["content"])
        except:
            logger.error(traceback.format_exc())

    def send_initialize_request(self):
        logger.info("\n--- Send initialize for {} ({})".format(self.project_path, self.server_info["name"]))
        
        self.sender.send_request("initialize", {
            "processId": os.getpid(),
            "rootPath": self.root_path,
            "clientInfo": {
                "name": "emacs",
                "version": get_emacs_version()
            },
            "rootUri": path_to_uri(self.project_path),
            "capabilities": self.get_capabilities(),
            "initializationOptions": self.server_info.get("initializationOptions", {})
        }, self.initialize_id, init=True)

    def get_capabilities(self):
        server_capabilities = self.server_info.get("capabilities", {})

        is_snippet_support = get_emacs_func_result("is-snippet-support")

        merge_capabilites = merge(server_capabilities, {
            "workspace": {
                "configuration": True
            },
            "textDocument": {
                "completion": {
                    "completionItem": {
                        "snippetSupport": False if not is_snippet_support else True,
                        "deprecatedSupport": True,
                        "tagSupport": {
                            "valueSet": [
                                1
                            ]
                        }
                    }
                },
                "codeAction": {
                    "dynamicRegistration": False,
                    "codeActionLiteralSupport": {
                        "codeActionKind": {
                            "valueSet": [
                                "quickfix",
                                "refactor",
                                "refactor.extract",
                                "refactor.inline",
                                "refactor.rewrite",
                                "source",
                                "source.organizeImports"
                            ]
                        }
                    },
                    "isPreferredSupport": True
                }
            }
        })

        return merge_capabilites

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
        
    def send_did_rename_files_notification(self, old_filepath, new_filepath):
        self.sender.send_notification("workspace/renameFiles", {
            "files": [{
                "oldUri": path_to_uri(old_filepath),
                "newUri": path_to_uri(new_filepath)
            }]
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

    def record_request_id(self, request_id: int, handler: Handler):
        self.request_dict[request_id] = handler

    def send_shutdown_request(self):
        self.sender.send_request("shutdown", {}, generate_request_id())

    def send_exit_notification(self):
        self.sender.send_notification("exit", {})

    def get_server_workspace_change_configuration(self):
        return {
            "settings": self.server_info["settings"]
        }

    def handle_workspace_configuration_request(self, name, request_id, params):
        settings = self.server_info.get("settings", {})            
        
        # We send empty message back to server if nothing in 'settings' of server.json file.
        if len(settings) == 0:
            self.sender.send_response(request_id, [])
            return
        
        # Otherwise, send back section value or default settings.
        items = []
        for p in params["items"]:
            section = p.get("section", self.server_info["name"])
            items.append(settings.get(section, {}))
        self.sender.send_response(request_id, items)

    def handle_recv_message(self, message: dict):
        if "error" in message:
            logger.error("\n--- Recv message (error):")
            logger.error(json.dumps(message, indent=3))
            
            error_message = message["error"]["message"]
            if error_message == "Unhandled method completionItem/resolve":
                self.completion_resolve_provider = False
            elif error_message == "Unhandled method textDocument/prepareRename":
                self.rename_prepare_provider = False
            elif error_message == "Unhandled method textDocument/codeAction":
                self.code_action_provider = False
            elif error_message == "Unhandled method textDocument/formatting":
                self.code_format_provider = False
            elif error_message == "Unhandled method textDocument/signatureHelp":
                self.signature_help_provider = False
            else:
                message_emacs(error_message)
            
            return

        if "id" in message:
            if "method" in message:
                # server request
                logger.info("\n--- Recv request ({}): {}".format(message["id"], message["method"]))
            else:
                # server response
                if message["id"] in self.request_dict:
                    method = self.request_dict[message["id"]].method
                    logger.info("\n--- Recv response {} ({}): {}".format(self.server_info["name"], message["id"], method))
                else:
                    logger.info("\n--- Recv response {} ({})".format(self.server_info["name"], message["id"]))
        else:
            if "method" in message:
                # server notification
                logger.info("\n--- Recv notification: %s", message["method"])
            else:
                # others
                logger.info("\n--- Recv message")

        if "method" in message and message["method"] == "textDocument/publishDiagnostics":
            filepath = uri_to_path(message["params"]["uri"])
            if is_in_path_dict(self.files, filepath):
                file_action = get_from_path_dict(self.files, filepath)
                file_action.diagnostics = message["params"]["diagnostics"]
        
        logger.debug(json.dumps(message, indent=3))

        if "id" in message:
            if message["id"] == self.initialize_id:
                # STEP 2: tell LSP server that client is ready.
                # We need wait LSP server response 'initialize', then we send 'initialized' notification.
                try:
                    # We pick up completion trigger characters from server.
                    # But some LSP server haven't this value, such as html/css LSP server.
                    self.completion_trigger_characters = message["result"]["capabilities"]["completionProvider"]["triggerCharacters"]
                except Exception:
                    pass

                try:
                    self.completion_resolve_provider = message["result"]["capabilities"]["completionProvider"]["resolveProvider"]
                except Exception:
                    pass

                try:
                    self.rename_prepare_provider = message["result"]["capabilities"]["renameProvider"]["prepareProvider"]
                except Exception:
                    pass

                try:
                    self.code_action_provider = message["result"]["capabilities"]["codeActionProvider"]
                    self.code_action_kinds = message["result"]["capabilities"]["codeActionProvider"]["codeActionKinds"]
                except Exception:
                    pass

                try:
                    self.code_format_provider = message["result"]["capabilities"]["documentFormattingProvider"]
                except Exception:
                    pass
                
                try:
                    self.signature_help_provider = message["result"]["capabilities"]["signatureHelpProvider"]
                except Exception:
                    pass
                
                self.sender.send_notification("initialized", {}, init=True)

                # STEP 3: Configure LSP server parameters.
                # After 'initialized' message finish, we should send 'workspace/didChangeConfiguration' notification.
                # The setting parameters of each language server are different.
                self.sender.send_notification("workspace/didChangeConfiguration",
                                              self.get_server_workspace_change_configuration(), init=True)

                self.sender.initialized.set()

                # Notify user server is ready.
                message_emacs("Start LSP server ({}) for {} with '{}' mode, enjoy hacking!".format(
                    self.server_info["name"],
                    self.root_path,
                    "project" if os.path.isdir(self.root_path) else "single-file"
                ))
            else:
                if "method" not in message and message["id"] in self.request_dict:
                    handler = self.request_dict[message["id"]]
                    handler.handle_response(
                        request_id=message["id"],
                        response=message["result"],
                    )
                else:
                    if message["method"] == "workspace/configuration":
                        self.handle_workspace_configuration_request(message["method"], message["id"], message["params"])
                    elif message["method"] == "workspace/applyEdit":
                        eval_in_emacs("lsp-bridge-workspace-apply-edit", message["params"]["edit"])

    def close_file(self, filepath):
        # Send didClose notification when client close file.
        if is_in_path_dict(self.files, filepath):
            self.send_did_close_notification(filepath)
            remove_from_path_dict(self.files, filepath)

        # We need shutdown LSP server when last file closed, to save system memory.
        if len(self.files) == 0:
            self.send_shutdown_request()
            self.send_exit_notification()

            self.message_queue.put({
                "name": "server_process_exit",
                "content": self.server_name
            })

            # Don't need to wait LSP server response, kill immediately.
            if self.lsp_subprocess is not None:
                try:
                    os.kill(self.lsp_subprocess.pid, 9)
                except ProcessLookupError:
                    logger.info("\n--- Lsp server process {} already exited!".format(self.lsp_subprocess.pid))
