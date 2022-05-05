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

from PyQt6 import QtCore
from PyQt6.QtCore import QObject, QTimer
from core.utils import get_command_result, eval_in_emacs
import os
import random

class FileAction(QObject):

    updatePosition = QtCore.pyqtSignal(str, int, int)
    popupCompletionItems = QtCore.pyqtSignal(str, str, list)
    
    def __init__(self, filepath):
        QObject.__init__(self)

        for name in ["find_define", "find_references", "prepare_rename", "rename", "completion"]:
            self.build_request_function(name)
            
        self.request_dict = {}

        self.filepath = filepath
        self.completion_request_list = []
        self.find_define_request_list = []
        self.find_references_request_list = []
        self.prepare_rename_request_list = []
        self.rename_request_list = []

        self.last_change_file_time = -1
        self.last_change_file_line_text = ""
        
        self.last_change_cursor_time = -1
        
        self.completion_prefix_string = ""
        
        self.popup_x = -1
        self.popup_y = -1

        self.version = 1

        self.try_completion_timer = None

        self.lsp_server = None
        self.lsp_server_type = "pyright"

        self.initialize_id = self.generate_request_id()

        dir_path = os.path.dirname(filepath)
        self.project_path = filepath
        if get_command_result("cd {} ; git rev-parse --is-inside-work-tree".format(dir_path)) == "true":
            self.project_path = get_command_result("cd {} ; git rev-parse --show-toplevel".format(dir_path))

    def generate_request_id(self):
        return abs(random.getrandbits(16))

    def get_lsp_server_name(self):
        return "{}#{}".format(self.project_path, self.lsp_server_type)

    def change_file(self, start_row, start_character, end_row, end_character, range_length, change_text, row, column, before_char, line_text):
        if self.lsp_server is not None:
            self.lsp_server.send_did_change_notification(
                self.filepath, self.version, start_row, start_character, end_row, end_character, range_length, change_text)

        self.version += 1

        if self.try_completion_timer is not None and self.try_completion_timer.isActive():
            self.try_completion_timer.stop()

        import time
        current_time = time.time()

        self.last_change_file_time = current_time
        self.last_change_file_line_text = line_text

        self.try_completion_timer = QTimer().singleShot(100, lambda : self.completion(row, column, before_char))

    def change_cursor(self, x, y):
        import time
        current_time = time.time()
        
        self.popup_x = x
        self.popup_y = y
        
        self.updatePosition.emit(self.filepath, self.popup_x, self.popup_y)

        self.last_change_cursor_time = current_time

    def build_request_function(self, name):
        def _do(*args):
            request_id = self.generate_request_id()
            getattr(self, "{}_request_list".format(name)).append(request_id)
            
            self.request_dict[request_id] = {
                "last_change_file_time": self.last_change_file_time,
                "last_change_cursor_time": self.last_change_cursor_time
            }

            if self.lsp_server is not None:
                args = (request_id, self.filepath, name) + args
                getattr(self.lsp_server, "send_{}_request".format(name))(*args)

        setattr(self, name, _do)

    def handle_response_message(self, request_id, request_type, response_result):
        if request_type == "completion":
            self.handle_completion_response(request_id, response_result)
        elif request_type == "find_define":
            self.handle_find_define_response(request_id, response_result)
        elif request_type == "find_references":
            self.handle_find_references_response(request_id, response_result)
        elif request_type == "prepare_rename":
            self.handle_prepare_rename_response(request_id, response_result)
        elif request_type == "rename":
            self.handle_rename_response(request_id, response_result)
                
    def handle_completion_response(self, request_id, response_result):
        if (request_id == self.completion_request_list[-1] and 
            self.request_dict[request_id]["last_change_file_time"] == self.last_change_file_time and 
            self.request_dict[request_id]["last_change_cursor_time"] == self.last_change_cursor_time):
            self.completion_prefix_string = self.last_change_file_line_text[self.last_change_file_line_text.rfind(".") + 1:]
        
            completion_items = []
            for item in response_result["items"]:
                if item["label"].startswith(self.completion_prefix_string) and item["label"] != self.completion_prefix_string:
                    completion_items.append({
                        "label": item["label"],
                        "type": item["kind"]
                    })
            self.popupCompletionItems.emit(self.filepath, self.completion_prefix_string, completion_items)
            
    def handle_find_define_response(self, request_id, response_result):
        if (request_id == self.find_define_request_list[-1] and 
            self.request_dict[request_id]["last_change_file_time"] == self.last_change_file_time and 
            self.request_dict[request_id]["last_change_cursor_time"] == self.last_change_cursor_time):
            try:
                file_info = response_result[0]
                filepath = file_info["uri"][len("file://"):]
                row = file_info["range"]["start"]["line"]
                column = file_info["range"]["start"]["character"]
                eval_in_emacs("lsp-bridge-jump-to-define", [filepath, row, column])
            except:
                print("* Failed information about find_define response.")
                import traceback
                traceback.print_exc()
                
    def handle_find_references_response(self, request_id, response_result):
        if request_id == self.find_references_request_list[-1]:
            print(response_result)
            
    def handle_prepare_rename_response(self, request_id, response_result):
        if request_id == self.prepare_rename_request_list[-1]:
            print(response_result)
            
    def handle_rename_response(self, request_id, response_result):
        if request_id == self.rename_request_list[-1]:
            print(response_result)
