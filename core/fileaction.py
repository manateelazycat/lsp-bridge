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
from typing import Dict, Tuple, TYPE_CHECKING

from core.lspserver import LspServer
from core.utils import *
from core.handler import *

if TYPE_CHECKING:
    from lsp_bridge import LspBridge


class FileAction:
    def __init__(self, filepath, lang_server_info, lsp_server, lsp_bridge, external_file_link=None):
        # Init.
        self.filepath = filepath
        self.lang_server_info = lang_server_info
        self.lsp_server: LspServer = lsp_server
        self.lsp_bridge: "LspBridge" = lsp_bridge
        self.external_file_link = external_file_link

        self.request_dict = {}
        self.last_change_file_time = -1.0
        self.last_change_file_before_cursor_text = ""
        self.last_change_cursor_time = -1.0
        self.version = 1
        
        self.last_completion_candidates = []
        
        self.completion_items = {}

        self.try_completion_timer = None
        self.try_signature_help_timer = None
        
        self.diagnostics = []

        # Initialize handlers.
        self.handlers: Dict[str, Handler] = dict()
        logger.debug("Handlers: " + pprint.pformat(Handler.__subclasses__()))
        for handler_cls in Handler.__subclasses__():
            self.handlers[handler_cls.name] = handler_cls(self)

        (self.enable_auto_import, self.enable_signature_help) = get_emacs_vars([
            "lsp-bridge-enable-auto-import",
            "lsp-bridge-enable-signature-help"
        ])

        self.lsp_server.attach(self)

    @property
    def last_change(self) -> Tuple[float, float]:
        """Return the last change information as a tuple."""
        return self.last_change_file_time, self.last_change_cursor_time

    def call(self, method, *args, **kwargs):
        """Call any handler or method of file action."""
        if method in self.handlers:
            return self.handlers[method].send_request(*args, **kwargs)
        getattr(self, method)(*args, **kwargs)

    def change_file(self, start, end, range_length, change_text, position, before_char, before_cursor_text, completion_visible):
        # Send didChange request to LSP server.
        self.lsp_server.send_did_change_notification(self.filepath, self.version, start, end, range_length, change_text)

        self.version += 1

        # Try cancel expired completion timer.
        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        # Record last change information.
        self.last_change_file_time = time.time()
        self.last_change_file_before_cursor_text = before_cursor_text

        # Send textDocument/completion 100ms later.
        self.try_completion_timer = threading.Timer(0.1, lambda : self.try_completion(position, before_char, completion_visible))
        self.try_completion_timer.start()
        
    def try_completion(self, position, before_char, completion_visible):
        # Only send textDocument/completion request when match one of following rules:
        # 1. Character before cursor is match completion trigger characters.
        # 2. Completion UI is invisible.
        # 3. Last completion candidates is empty.
        if ((before_char in self.lsp_server.completion_trigger_characters) or
            (not completion_visible) or
            len(self.last_completion_candidates) == 0):
            self.handlers["completion"].send_request(position, before_char)
            
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
        
    def completion_item_resolve(self, label, item_key):
        if item_key in self.completion_items:
            self.handlers["completion_item_resolve"].send_request(label, self.completion_items[item_key])
