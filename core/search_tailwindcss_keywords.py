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

from core.utils import eval_in_emacs, get_project_path

class SearchTailwindKeywords:
    
    def __init__(self) -> None:
        self.search_ticker = 0
        self.search_thread_queue = []
        self.search_max_number = 300
        
        self.file_path_dict = {}
        
        with open(os.path.join(os.path.dirname(__file__), "tailwind_css_keyword.txt")) as f:
            self.symbols = f.read().splitlines()
            
    def search(self, filepath: str, prefix: str):
        if filepath in self.file_path_dict:
            is_tailwind_project = self.file_path_dict[filepath]
            if is_tailwind_project:
                ticker = self.search_ticker + 1
                self.search_ticker = ticker
                
                search_thread = threading.Thread(target=lambda: self.search_symbols(prefix, ticker))
                search_thread.start()
                self.search_thread_queue.append(search_thread)
        else:
            # Search fuck tailwind config file.
            self.file_path_dict[filepath] = os.path.exists(os.path.join(get_project_path(filepath), "tailwind.config.js"))
        
    def match_symbol(self, prefix, prefix_regexp, symbol):
        return symbol.startswith(prefix) or symbol.replace("-", "").startswith(prefix) or prefix_regexp.match(symbol)
        
    def search_symbols(self, prefix: str, ticker: int):
        candidates = []
        prefix_regexp = re.compile(re.sub(r'([a-zA-Z0-9-_])', r'\1.*', re.escape(prefix)))
        for symbol in self.symbols:
            if self.match_symbol(prefix, prefix_regexp, symbol):
                candidates.append(symbol)
                    
                if len(candidates) > self.search_max_number:
                    break
                
        if ticker == self.search_ticker:
            eval_in_emacs("lsp-bridge-search-backend--record-items", "tailwind-keywords", candidates)
