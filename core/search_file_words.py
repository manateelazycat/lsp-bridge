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

import copy
import queue
import threading
import traceback
import re

from core.utils import *

class SearchFileWords:
    def __init__(self):
        self.files = {}
        self.search_files = set()
        self.search_content_dict = {}
        self.search_words_thread = None
        (self.max_number, self.fuzzy_match, self.fuzzy_match_threshold) = get_emacs_vars(["acm-backend-search-file-words-max-number", "acm-backend-search-file-words-enable-fuzzy-match", "acm-backend-search-file-words-enable-fuzzy-match-threshold"])
        self.search_words_queue = queue.Queue()
        self.search_words_dispatcher_thread = threading.Thread(target=self.search_dispatcher)
        self.search_words_dispatcher_thread.start()

    def index_files(self, filepaths):
        for filepath in filepaths:
            self.search_files.add(filepath)
        
        add_queue = False
        for filepath in filepaths:
            if filepath not in self.files:
                self.files[filepath] = set()
                add_queue = True
                
        if add_queue:
            self.search_words_queue.put("search_words")

    def index_file(self, filepath, content):
        self.search_files.add(filepath)
        self.search_content_dict[filepath] = content

        if filepath not in self.files:
            self.files[filepath] = set()

        self.search_words_queue.put("search_words")

    def load_file(self, filepath, from_file_server=False):
        try:
            if from_file_server:
                content = get_file_content_from_file_server(filepath)
            else:
                with open(filepath) as f:
                    content = f.read()
        except:
            return

        self.index_file(filepath, content)

    def change_buffer(self, buffer_name, start_pos, end_pos, change_text):
        start_pos = epc_arg_transformer(start_pos)
        end_pos = epc_arg_transformer(end_pos)

        if len(start_pos) == 0 or len(end_pos) == 0:
            return

        if buffer_name in self.search_content_dict:
            content = rebuild_content_from_diff(self.search_content_dict[buffer_name], start_pos, end_pos, change_text)
        else:
            content = get_emacs_func_result('get-buffer-content', buffer_name, True)

        self.index_file(buffer_name, content)

    def close_file(self, filepath):
        if filepath in self.files:
            if filepath in self.search_files:
                self.search_files.remove(filepath)
                
            if filepath in self.search_content_dict:
                del self.search_content_dict[filepath]
                
            del self.files[filepath]
    
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
                
            eval_in_emacs("lsp-bridge-search-backend--record-items", "search-file-words",
                          list(map(lambda word: {
                              "key": word,
                              "icon": "search",
                              "label": word,
                              "displayLabel": word,
                              "annotation": "Search Word",
                              "backend": "search-file-words"
                          }, candidates[:min(self.max_number, len(candidates))])))
        except:
            logger.error(traceback.format_exc())
            
    def search_word(self, prefix, all_words):
        candidates = []
        if self.fuzzy_match:
            from rapidfuzz import fuzz
            match_words = list(map(lambda word: {'word':word, 'ratio':fuzz.ratio(prefix.lower(), word.lower())}, all_words))
            match_words.sort(key=lambda x:x['ratio'], reverse=True)
            for word in match_words:
                if word['ratio'] >= self.fuzzy_match_threshold:
                    candidates.append(word['word'])
                else:
                    break
        else:
            match_words = list(filter(lambda word: word.lower().startswith(prefix.lower()), all_words))
            if prefix.isupper():
                candidates = list(map(lambda word: word.upper(), match_words))
            else:
                candidates = list(map(lambda word: prefix + word[len(prefix):], match_words))
            candidates.sort(key=len, reverse=False)
        return candidates
            
    def search_dispatcher(self):
        try:
            while True:
                message = self.search_words_queue.get(block=True)
                if message == "search_words":
                    search_files = copy.deepcopy(self.search_files)
                    search_content_dict = copy.deepcopy(self.search_content_dict)
                    
                    for search_file in search_files:
                        try:
                            if search_file in search_content_dict:
                                words = set(re.findall("[\w|-]+", search_content_dict[search_file]))
                            else:
                                words = set(re.findall("[\w|-]+", open(search_file).read()))
                        except (FileNotFoundError, UnicodeDecodeError):
                            continue
                        filter_words = set(map(lambda word: re.sub('[^A-Za-z0-9-_]+', '', word),
                                               set(filter(self.filter_word, words))))
                        filter_words.discard("")
                        self.files[search_file] = filter_words
                        
                    self.search_files.clear()
        except:
            logger.error(traceback.format_exc())

    def filter_word(self, word: str):
        return len(word) > 3 and (not word.isnumeric())
