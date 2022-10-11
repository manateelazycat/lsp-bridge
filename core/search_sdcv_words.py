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

from core.utils import get_emacs_var, message_emacs, eval_in_emacs


class SearchSdcvWords:
    
    def __init__(self) -> None:
        self.pystardict_is_installed = True
        
        self.search_max_number = get_emacs_var("acm-backend-search-sdcv-words-candidates-max-number")
        self.search_ticker = 0
        self.search_thread_queue = []
        self.words = {}
        
        self.build_words_thread = threading.Thread(target=self.build_words)
        self.build_words_thread.start()
        
    def search(self, prefix: str):
        ticker = self.search_ticker + 1
        self.search_ticker = ticker
        
        search_thread = threading.Thread(target=lambda: self.search_words(prefix, ticker))
        search_thread.start()
        self.search_thread_queue.append(search_thread)
        
    def build_words(self):
        try:
            from pystardict import Dictionary    # type: ignore
            
            if len(self.words) == 0:
                dictionary_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "resources", "kdic-ec-11w")
                start_dictionary = Dictionary(dictionary_path)
                
                candidates = []
                for word in start_dictionary.keys():
                    is_english_word = all(ord(char) < 128 for char in word)
                    if is_english_word:
                        first_line_translation = start_dictionary.dict[word].split()[0]
                        no_phonetic_translation = first_line_translation.split(">")[-1]
                
                        candidate_word  = word.lower().replace('\"', ' ')
                        candidate_translation = no_phonetic_translation.strip().replace('\"', ' ')
            
                        self.words[candidate_word] = candidate_translation
                        
            self.pystardict_is_installed = True
        except:
            self.pystardict_is_installed = False
            
    def adjust_word_case(self, prefix: str, candidate: str):
        if len(prefix) > 1 and prefix.isupper():
            return candidate.upper()
        elif prefix.istitle():
            words = candidate.split()
            if len(words) > 1:
                return words[0].title() + " " + " ".join(list(map(lambda w: w.lower(), words[1:])))
            else:
                return candidate.title()
        else:
            return candidate
                    
    def search_words(self, prefix: str, ticker: int):
        if self.pystardict_is_installed:
            candidates = []
            for word, translation in self.words.items():
                if word.startswith(prefix.lower()):
                    candidate = {
                        "key": word,
                        "icon": "translation",
                        "label": word,
                        "display-label": self.adjust_word_case(prefix, word),
                        "annotation": translation,
                        "backend": "search-sdcv-words"
                    }
                    candidates.append(candidate)
                        
                    if len(candidates) > self.search_max_number:
                        break
                    
            if ticker == self.search_ticker:
                eval_in_emacs("lsp-bridge-search-sdcv-words--record-items", candidates)
        else:
            message_emacs("You need install PyStarDict with command 'pip3 install PyStarDict' to use English Helper feature.")
            self.build_words()
