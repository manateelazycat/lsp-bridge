#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
# 
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: <lazycat.manatee@gmail.com> <lazycat.manatee@gmail.com>
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

import threading
import os
import re
import functools

from core.utils import eval_in_emacs, cmp

class SearchPaths:
    
    def __init__(self) -> None:
        self.search_ticker = 0
        self.search_thread_queue = []
        self.search_max_number = 300
        
    def search(self, filename: str, prefix: str):
        ticker = self.search_ticker + 1
        self.search_ticker = ticker
                
        search_thread = threading.Thread(target=lambda: self.search_symbols(filename, prefix, ticker))
        search_thread.start()
        self.search_thread_queue.append(search_thread)

    def match_symbol(self, prefix, symbol):
        return symbol.startswith(prefix) or symbol.replace("-", "").startswith(prefix) or prefix in symbol
        
    def search_symbols(self, filename: str, prefix: str, ticker: int):
        candidates = []

        for file in os.listdir(filename):
            if self.match_symbol(prefix.lower(), file.lower()):
                file_type = "dir" if os.path.isdir(os.path.join(filename, file)) else "file"

                candidates.append({
                    "key": file,
                    "icon": file_type,
                    "label": file,
                    "display-label": file,
                    "annotation": file_type.upper(),
                    "backend": "path"
                })
                    
                if len(candidates) > self.search_max_number:
                    break
                
        if ticker == self.search_ticker:
            eval_in_emacs("lsp-bridge-search-backend--record-items", "paths", sorted(candidates, key=functools.cmp_to_key(lambda a, b: self.sort_files(prefix, a, b))))

    def sort_files(self, prefix, file_a, file_b):
        file_a_starts_with_prefix = file_a["key"].startswith(prefix)
        file_b_starts_with_prefix = file_b["key"].startswith(prefix)

        if file_a_starts_with_prefix and file_b_starts_with_prefix:
            return self.sort_file(file_a, file_b)
        elif file_a_starts_with_prefix:
            return -1
        elif file_b_starts_with_prefix:
            return 1
        else:
            return self.sort_file(file_a, file_b)

    def sort_file(self, file_a, file_b):
        file_a_is_file_dir = file_a["icon"] == "dir"
        file_b_is_file_dir = file_b["icon"] == "dir"

        if file_a_is_file_dir and file_b_is_file_dir:
            return cmp(file_a["key"], file_b["key"])
        elif file_a_is_file_dir:
            return -1
        elif file_b_is_file_dir:
            return 1
        else:
            return cmp(file_a["key"], file_b["key"])
