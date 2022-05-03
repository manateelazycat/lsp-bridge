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

from PyQt6.QtCore import QObject
from core.utils import get_command_result
import os
import random

class FileAction(QObject):
    
    def __init__(self, filepath):
        QObject.__init__(self)
        
        self.filepath = filepath
        self.code_has_change = False
        self.completion_request_list = []
        self.find_define_request_list = []
        self.find_references_request_list = []
        self.rename_request_list = []
        
        self.lsp_server_type = "pyright"
        self.initialize_id = random.getrandbits(32)
        
        print("***** ", self.initialize_id)
        
        dir_path = os.path.dirname(filepath)
        self.project_path = filepath
        if get_command_result("cd {} ; git rev-parse --is-inside-work-tree".format(dir_path)) == "true":
            self.project_path = get_command_result("cd {} ; git rev-parse --show-toplevel".format(dir_path))

    def get_lsp_server_name(self):
        return "{}#{}".format(self.project_path, self.lsp_server_type)
