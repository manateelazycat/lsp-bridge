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
import traceback
import re

from core.utils import get_emacs_vars, message_emacs, eval_in_emacs, logger, parse_json_content
from core.pystardict import Dictionary


class SearchSdcvWords:
    
    def __init__(self) -> None:
        [self.search_max_number, self.search_dictionary] = get_emacs_vars([
            "acm-backend-search-sdcv-words-candidates-max-number",
            "acm-backend-search-sdcv-words-dictionary"])
        self.search_ticker = 0
        self.search_thread_queue = []
        self.words = {}
        self.pinyin= {}
        
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
            if len(self.words) == 0:
                pinyin_dictionary_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "resources", "pinyin")
                if os.path.exists("{}.ifo".format(pinyin_dictionary_path)):
                    pinyin_dictionary = Dictionary(pinyin_dictionary_path, in_memory=True)
                    for key, value in pinyin_dictionary.items():
                        self.pinyin[key] = parse_json_content(value)

                if self.search_dictionary == "kdic-ec-11w":
                    dictionary_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "resources", self.search_dictionary)
                else:
                    dictionary_path = self.search_dictionary
                    
                if os.path.exists("{}.ifo".format(dictionary_path)):
                    start_dictionary = Dictionary(dictionary_path, in_memory=True)
                    
                    candidates = []
                    for word in start_dictionary.keys():
                        first_line_translation = start_dictionary.dict[word].split()[0]
                        no_phonetic_translation = first_line_translation.split(">")[-1]
                    
                        candidate_word  = word.lower().replace('\"', ' ')
                        candidate_translation = no_phonetic_translation.strip().replace('\"', ' ')
                    
                        self.words[candidate_word] = candidate_translation
                else:
                    message_emacs("StarDic dictionary {}.ifo is not exists".format(dictionary_path))
        except:
            logger.error(traceback.format_exc())
            
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
        candidates = []
        
        prefix_regexp = re.compile(".*".join(prefix))
        if len(prefix.lower()) > 3 and prefix.lower() in self.pinyin:
            for word, translation in self.pinyin[prefix.lower()].items():
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

        for word, translation in self.words.items():
            if word.startswith(prefix.lower()) or prefix_regexp.match(word):
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
            eval_in_emacs("lsp-bridge-search-backend--record-items", "sdcv-words", sorted(candidates, key=lambda item: len(item["label"])))
