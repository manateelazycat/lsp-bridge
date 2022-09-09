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

import queue
import threading
import traceback
import re

from core.utils import *

class SearchFileWords:
    def __init__(self):
        self.files = {}
        self.search_files = set()
        self.search_files_mutex = threading.Lock()
        self.search_words_thread = None
        
        self.search_words_queue = queue.Queue()
        self.search_words_dispatcher_thread = threading.Thread(target=self.search_dispatcher)
        self.search_words_dispatcher_thread.start()
    
    def close_file(self, filepath):
        if filepath in self.files:
            self.search_files_mutex.acquire()
            if filepath in self.search_files:
                self.search_files.remove(filepath)
            self.search_files_mutex.release()
            
            del self.files[filepath]
    
    def change_file(self, filepath):
        self.search_files_mutex.acquire()
        self.search_files.add(filepath)
        self.search_files_mutex.release()
        
        if filepath not in self.files:
            self.files[filepath] = set()
            self.search_words_queue.put("search_words")
    
    def search(self, prefix: str):
        self.search_words_thread = threading.Thread(target=lambda : self.search_words_from_files(prefix))
        self.search_words_thread.start()
        
    def search_words_from_files(self, prefix: str):
        try:
            all_words = set()
            for file, words in self.files.items():
                all_words = all_words | words
                
            search_candidates = self.search_word(prefix, all_words)
            candidates = []
            if len(search_candidates) > 0:
                candidates = search_candidates
            elif ("-" in prefix) and (not prefix.endswith("-")):
                search_prefix = prefix.split("-")[-1]
                candidates = self.search_word(search_prefix, all_words)
                
                candidates = list(map(lambda word: prefix[:-len(search_prefix)] + word, candidates))
            elif ("_" in prefix and (not prefix.endswith("_"))):
                search_prefix = prefix.split("_")[-1]
                candidates = self.search_word(search_prefix, all_words)
                
                candidates = list(map(lambda word: prefix[:-len(search_prefix)] + word, candidates))
                

            eval_in_emacs("lsp-bridge-record-search-words-items", candidates[:min(3, len(candidates))])
        except:
            logger.error(traceback.format_exc())
            
    def search_word(self, prefix, all_words):
        match_words = list(filter(lambda word: word.startswith(prefix.lower()), all_words))
        candidates = []
        if prefix.isupper():
            candidates = list(map(lambda word: word.upper(), match_words))
        else:
            candidates = list(map(lambda word: prefix + word[len(prefix):], match_words))
            
        candidates.sort(key=len, reverse=False)
        
        return candidates
            
    def rebuild_cache(self):
        if len(self.search_files) > 0:
            self.search_words_queue.put("search_words")
    
    def search_dispatcher(self):
        try:
            while True:
                message = self.search_words_queue.get(block=True)
                if message == "search_words":
                    self.search_files_mutex.acquire()
                    for search_file in self.search_files:
                        try:
                            words = set(re.findall("[\w|-]+", open(search_file).read()))
                        except (FileNotFoundError, UnicodeDecodeError):
                            continue
                        filter_words = set(map(lambda word: re.sub('[^A-Za-z0-9-_]+', '', word),
                                               set(filter(self.filter_word, words))))
                        filter_words.discard("")
                        self.files[search_file] = filter_words
                        
                    self.search_files.clear()
                    self.search_files_mutex.release()
        except:
            logger.error(traceback.format_exc())

    def filter_word(self, word: str):
        return len(word) > 3 and (not word.isnumeric())
