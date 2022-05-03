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
from core.utils import get_command_result
import os
import random

class FileAction(QObject):
    
    completion = QtCore.pyqtSignal(int, str, str, int, int, str)
    change = QtCore.pyqtSignal(int, str, str, int, int, str)
    
    def __init__(self, filepath):
        QObject.__init__(self)
        
        self.filepath = filepath
        self.completion_request_list = []
        self.find_define_request_list = []
        self.find_references_request_list = []
        self.rename_request_list = []
        
        self.code_has_change = False
        self.last_change_time = -1
        self.last_change_row = -1
        self.last_change_column = -1
        self.last_change_char = ""
        
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
    
    def change_file(self, row, column, char):
        self.change.emit(self.generate_request_id(), self.get_lsp_server_name(), self.filepath, row, column, char)
        
        import time
        current_time = time.time()
        
        self.code_has_change = True
        self.last_change_time = current_time
        self.last_change_row = row
        self.last_change_column = column
        self.last_change_char = char

        QTimer().singleShot(500, lambda : self.try_completion(current_time, row, column, char))
        
    def try_completion(self, time, row, column, char):
        if time == self.last_change_time and row == self.last_change_row and column == self.last_change_column and self.last_change_char == char:
            self.completion.emit(self.generate_request_id(), self.get_lsp_server_name(), self.filepath, row, column, char)
