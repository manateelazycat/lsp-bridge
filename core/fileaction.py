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
import pprint
import threading
import time
import os
from typing import Dict, Tuple

from core.utils import *
from core.handler import *


class FileAction:

    def __init__(self, filepath, project_path, lang_server):
        # Init.
        self.filepath = filepath
        self.lsp_location_link = None
        self.project_path = project_path
        self.request_dict = {}
        self.last_change_file_time = -1.0
        self.last_change_file_before_cursor_text = ""
        self.last_change_cursor_time = -1.0
        self.version = 1

        self.try_completion_timer = None
        self.try_signature_help_timer = None

        # Read language server information.
        self.lang_server_info = self.load_lang_server_info(lang_server)

        # Initialize handlers.
        self.handlers: Dict[str, Handler] = dict()
        logger.debug("Handlers: " + pprint.pformat(Handler.__subclasses__()))
        for handler_cls in Handler.__subclasses__():
            self.handlers[handler_cls.name] = handler_cls(self)

        self.lsp_server = None

        (self.enable_auto_import, self.enable_signature_help) = get_emacs_vars([
            "lsp-bridge-enable-auto-import",
            "lsp-bridge-enable-signature-help"
        ])

    @property
    def last_change(self) -> Tuple[float, float]:
        """Return the last change information as a tuple."""
        return self.last_change_file_time, self.last_change_cursor_time

    def call(self, method, *args, **kwargs):
        """Call any handler or method of file action."""
        if method in self.handlers:
            return self.handlers[method].send_request(*args, **kwargs)
        getattr(self, method)(*args, **kwargs)

    def load_lang_server_info(self, lang_server):
        lang_server_info_path = ""
        if os.path.exists(lang_server) and os.path.sep in lang_server:
            # If lang_server is real file path, we load the LSP server configuration from the user specified file.
            lang_server_info_path = lang_server
        else:
            # Otherwise, we load LSP server configuration from file lsp-bridge/langserver/lang_server.json.
            lang_server_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), "langserver")
            lang_server_file_path_current = os.path.join(lang_server_dir,
                                                         "{}_{}.json".format(lang_server, get_os_name()))
            lang_server_file_path_default = os.path.join(lang_server_dir, "{}.json".format(lang_server))

            lang_server_info_path = lang_server_file_path_current if os.path.exists(
                lang_server_file_path_current) else lang_server_file_path_default

        with open(lang_server_info_path, encoding="utf-8") as f:
            import json
            return json.load(f)

    def get_lsp_server_name(self):
        # We use project path and LSP server type as unique name.
        return "{}#{}".format(path_as_key(self.project_path), self.lang_server_info["name"])

    def change_file(self, start, end, range_length, change_text, position, before_char, before_cursor_text):
        # Send didChange request to LSP server.
        if self.lsp_server is not None:
            self.lsp_server.send_did_change_notification(
                self.filepath, self.version, start, end, range_length, change_text)
        else:
            # Please report bug if you got this message.
            logger.error("IMPOSSIBLE HERE: change_file %s %s", self.filepath, self.lsp_server)

        self.version += 1

        # Try cancel expired completion timer.
        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        # Record last change information.
        self.last_change_file_time = time.time()
        self.last_change_file_before_cursor_text = before_cursor_text

        # Send textDocument/completion 100ms later.
        self.try_completion_timer = threading.Timer(
            0.1, lambda: self.handlers["completion"].send_request(position, before_char)
        )
        self.try_completion_timer.start()

    def change_cursor(self, position):
        # Record change cursor time.
        self.last_change_cursor_time = time.time()

        if self.enable_signature_help:
            # Try cancel expired signature help timer.
            if self.try_signature_help_timer is not None and self.try_signature_help_timer.is_alive():
                self.try_signature_help_timer.cancel()
            
            # Send textDocument/signatureHelp 200ms later.
            self.try_signature_help_timer = threading.Timer(
                0.2, lambda: self.handlers["signature_help"].send_request(position)
            )
            self.try_signature_help_timer.start()

    def save_file(self):
        self.lsp_server.send_did_save_notification(self.filepath)

    def handle_server_response_message(self, request_id, request_type, response):
        self.handlers[request_type].handle_response(request_id, response)
