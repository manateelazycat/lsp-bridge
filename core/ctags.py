#! /usr/bin/env python
# -*- coding: utf-8 -*-

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

import subprocess
import threading
import re
import os

from core.utils import *

DEFAULT_FILTER_CMD = '(not (or (and $extras ((string->regexp "(^|,) ?(anonymous|reference)(,|$)" :case-fold false) $extras)) (or (and $extras ((string->regexp "(^|,) ?(inputFile)(,|$)" :case-fold false) $extras)) (and $kind ((string->regexp "^(file|F)$" :case-fold false) $kind))) false))'

DEFAULT_SORTER_CMD = '(<or> (if (and $name &name) (<> (length $name) (length &name)) 0) (if (and $name &name) (<> $name &name) 0))'

# for Python 3.9-
def remove_prefix(text, prefix):
    if text.startswith(prefix):
        return text[len(prefix):]
    return text

class Ctags:    
    def __init__(self) -> None:
        self.current_cursor_offset = 0
        self.lock = threading.RLock()

    def run_cmd_in_path(self, cmd, filename, in_shell=False):
        if os.path.isfile(filename):
            cwd = os.path.dirname(filename)
        else:
            cwd = filename

        result = subprocess.run(cmd, cwd=cwd, shell=in_shell, capture_output=True, text=True)

        # Get the output as a string
        output = ""
        status = result.returncode
        if status != 0:
            logger.error(result.stderr)
        else:
            output = result.stdout

        lines = output.split('\n')
        lines = [l for l in lines if l != '']

        return lines

    def parse_tag_line(self, line):
        if line == '':
            return None

        tag = {}
        entry = line.rsplit(';"', 1)

        normal_field = entry[0].split('\t', 2)
        tag['tagname'] = normal_field[0]
        tag['tagfile'] = normal_field[1]
        tag['tagaddress'] = normal_field[2]

        if len(entry) == 2:
            for field in entry[1].split('\t')[1:]:
                field_entry = field.split(':', 1)
                if len(field_entry) == 2:
                    tag.update(dict([field_entry]))
                else:
                    tag.update({"kind": field_entry})

        return tag

    def make_tag_annotation(self, tag):
        kind = tag.get("kind", "")
        typeref = tag.get("typeref", "")
        scope = tag.get("scope", "")
        extras = tag.get("extras", "")
        reference = "<R>" if "reference" in extras else ""

        typeref = remove_prefix(typeref, "typename:")
        typeref = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', typeref)
        scope = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', scope)

        annotation = (reference, 
                      kind, 
                      "/" if (kind != "" and typeref != "") else "",
                      typeref,
                      "@" if (scope != "") else "",
                      scope)

        return ''.join(annotation)

    def make_ctags_acm_candidate(self, tag: dict):
        candidate = {}
        tagname = tag["tagname"]

        kind = tag.get("kind", "NOKIND")
        signature = tag.get("signature", "NOSIGN")
        annotation = self.make_tag_annotation(tag)

        candidate["key"] = tagname
        candidate["icon"] = kind
        candidate["label"] = tagname
        candidate["displayLabel"] = tagname
        candidate["annotation"] = annotation
        candidate["backend"] = "ctags"

        return candidate

    def locate_dominating_file(self, file_or_dir, filename):
        current_dir = os.path.abspath(file_or_dir)
        
        while True:
            if os.path.exists(os.path.join(current_dir, filename)):
                return os.path.join(current_dir, filename)
            
            if current_dir == os.path.dirname(current_dir):
                break

            current_dir = os.path.dirname(current_dir)
            
        return None

    def make_complete(self, symbol, filename, cursor_offset):
        self.lock.acquire()
        try:
            self.current_cursor_offset = cursor_offset
        finally:
            self.lock.release()

        if not filename:
            return

        tagsfile = self.locate_dominating_file(filename, "tags")
        if not tagsfile:
            return
        
        cmd = self.readtags_get_cmd(tagsfile, symbol, "prefix", False, DEFAULT_FILTER_CMD, DEFAULT_SORTER_CMD, "")

        lines = self.run_cmd_in_path(cmd, filename)
        
        tags = map(self.parse_tag_line, lines)
        candidates = map(self.make_ctags_acm_candidate, tags)

        self.dispatch(list(candidates), cursor_offset)

    def dispatch(self, candidates, cursor_offset):
        self.lock.acquire()
        try:
            if self.current_cursor_offset == cursor_offset:
                eval_in_emacs("lsp-bridge-search-backend--record-items", "ctags", candidates)
        finally:
            self.lock.release()

    def readtags_get_cmd(self, tagsfile, name, match, case_fold, filter, sorter, action):
        extras = ("-Ene" +
                  ("" if match == "exact" else "p") +
                  ("i" if case_fold else ""))
        cmd = []

        # program name        
        cmd.append("readtags")

        # read from this tags file
        cmd.append("-t")
        cmd.append(tagsfile)

        # filter expresion
        if filter:
            cmd.append("-Q")
            cmd.append(filter)
        if sorter:
            cmd.append("-S")
            cmd.append(sorter)

        # extras arguments
        cmd.append(extras)

        # action
        if action:
            cmd.append(action)
        else:
            if not name:
                cmd.append("-l")
            else:
                cmd.append("-")
                cmd.append(name)

        return cmd
