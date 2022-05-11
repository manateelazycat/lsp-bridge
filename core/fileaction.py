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

import os
import random
import threading
import time

from core.utils import (eval_in_emacs, generate_request_id, get_command_result,
                        get_emacs_var, uri_to_path)

KIND_MAP = ["", "Text", "Method", "Function", "Constructor", "Field",
            "Variable", "Class", "Interface", "Module", "Property",
            "Unit" , "Value" , "Enum", "Keyword" , "Snippet", "Color",
            "File", "Reference", "Folder", "EnumMember", "Constant",
            "Struct", "Event", "Operator", "TypeParameter"]

REFERENCE_PATH = '\033[95m'
REFERENCE_TEXT = '\033[94m'
REFERENCE_ENDC = '\033[0m'

class FileAction(object):

    def __init__(self, filepath, lang_server):
        object.__init__(self)

        # Build request functions.
        for name in ["find_define", "find_references", "prepare_rename", "rename", "completion"]:
            self.build_request_function(name)

        # Init.
        self.filepath = filepath
        self.request_dict = {}
        self.completion_request_list = []
        self.find_define_request_list = []
        self.find_references_request_list = []
        self.prepare_rename_request_list = []
        self.rename_request_list = []
        self.last_change_file_time = -1
        self.last_change_file_before_cursor_text = ""
        self.last_change_cursor_time = -1
        self.completion_prefix_string = ""
        self.version = 1
        self.try_completion_timer = None

        # Read language server information.
        self.lang_server_info = None
        self.lang_server_info_path = ""
        if os.path.exists(lang_server) and os.path.sep in lang_server:
            # If lang_server is real file path, we load the LSP server configuration from the user specified file.
            self.lang_server_info_path = lang_server
        else:
            # Otherwise, we load LSP server configuration from file lsp-bridge/langserver/lang_server.json.
            self.lang_server_info_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "langserver", "{}.json".format(lang_server))

        with open(self.lang_server_info_path) as f:
            import json
            self.lang_server_info = json.load(f)

        self.lsp_server = None

        # Generate initialize request id.
        self.initialize_id = generate_request_id()

        # Project path is same as file path if open an isolated file.
        # Otherwise use git root patch as project path.
        dir_path = os.path.dirname(filepath)
        self.project_path = filepath
        if get_command_result("git rev-parse --is-inside-work-tree", dir_path) == "true":
            self.project_path = get_command_result("git rev-parse --show-toplevel", dir_path)

    def get_lsp_server_name(self):
        # We use project path and LSP server type as unique name.
        return "{}#{}".format(self.project_path, self.lang_server_info["name"])

    def change_file(self, start_row, start_character, end_row, end_character, range_length, change_text, row, column, before_char, before_cursor_text):
        # Send didChange request to LSP server.
        if self.lsp_server is not None:
            self.lsp_server.send_did_change_notification(
                self.filepath, self.version, start_row, start_character, end_row, end_character, range_length, change_text)
        else:
            # Please report bug if you got this message.
            print("IMPOSSIBLE HERE: change_file ", self.filepath, self.lsp_server)

        self.version += 1

        # Try cancel expired completion timer.
        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        # Record last change information.
        self.last_change_file_time = time.time()
        self.last_change_file_before_cursor_text = before_cursor_text

        # Send textDocument/completion 100ms later.
        self.try_completion_timer = threading.Timer(0.1, lambda : self.completion(row, column, before_char))
        self.try_completion_timer.start()

    def change_cursor(self):
        # Record change cursor time.
        self.last_change_cursor_time = time.time()

    def build_request_function(self, name):
        def _do(*args):
            request_id = generate_request_id()
            getattr(self, "{}_request_list".format(name)).append(request_id)

            # Cache last change information to compare after receive LSP server response message.
            self.request_dict[request_id] = {
                "last_change_file_time": self.last_change_file_time,
                "last_change_cursor_time": self.last_change_cursor_time
            }

            if self.lsp_server is not None:
                args = (request_id, self.filepath, name) + args
                getattr(self.lsp_server, "send_{}_request".format(name))(*args)

        setattr(self, name, _do)

    def handle_server_response_message(self, request_id, request_type, response_result):
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
        # Stop send completion items to client if request id expired, or change file, or move cursor.
        if (request_id == self.completion_request_list[-1] and
            self.request_dict[request_id]["last_change_file_time"] == self.last_change_file_time and
            self.request_dict[request_id]["last_change_cursor_time"] == self.last_change_cursor_time):

            # Calcuate completion prefix string.
            self.completion_prefix_string = self.calc_completion_prefix_string()

            # Get completion items.
            completion_items = []
            kinds = []
            annotations = []
            documents = []

            if response_result is not None:
                for item in response_result["items"] if "items" in response_result else response_result:
                    completion_items.append(item["label"])
                    kinds.append(KIND_MAP[item["kind"]])
                    annotations.append(item["detail"] if "detail" in item else kinds[-1])
                    annotations[-1] = annotations[-1].replace(" ", "") #  HACK: space makes the number of args for emacs wrong
                    #  TODO: the situtation for documentation is complex
                    # documents.append(item["documentation"] if "documentation" in item else "")

            # Calcuate completion common string.
            completion_common_string = os.path.commonprefix(completion_items)

            # Push completion items to Emacs.
            if len(completion_items) == 1 and (self.completion_prefix_string == completion_common_string == completion_items[0]):
                # Clear completion items if user input last completion item.
                eval_in_emacs("lsp-bridge-record-completion-items", [self.filepath, self.completion_prefix_string,
                                                                     completion_common_string, [], [], []])
            else:
                eval_in_emacs("lsp-bridge-record-completion-items", [self.filepath, self.completion_prefix_string,
                                                                     completion_common_string, completion_items, kinds, annotations])

    def calc_completion_prefix_string(self):
        if self.last_change_file_before_cursor_text.endswith(" "):
            # Return "" if have blank character before cursor.
            return ""
        else:
            string_after_dot = self.last_change_file_before_cursor_text[self.last_change_file_before_cursor_text.rfind(".") + 1:]
            split_strings = string_after_dot.split()
            if len(split_strings) > 0:
                return split_strings[-1]
            else:
                return ""

    def handle_find_define_response(self, request_id, response_result):
        # Stop send jump define if request id expired, or change file, or move cursor.
        if (request_id == self.find_define_request_list[-1] and
            self.request_dict[request_id]["last_change_file_time"] == self.last_change_file_time and
            self.request_dict[request_id]["last_change_cursor_time"] == self.last_change_cursor_time):

            if response_result:
                try:
                    file_info = response_result[0]
                    filepath = uri_to_path(file_info["uri"])
                    row = file_info["range"]["start"]["line"]
                    column = file_info["range"]["start"]["character"]
                    eval_in_emacs("lsp-bridge-jump-to-define", [filepath, row, column])
                except:
                    print("* Failed information about find_define response.")
                    import traceback
                    traceback.print_exc()
            else:
                eval_in_emacs("message", ["Can't find define."])

    def handle_find_references_response(self, request_id, response_result):
        import linecache
        
        if request_id == self.find_references_request_list[-1]:
            references_dict = {}
            for uri_info in response_result:
                path = uri_to_path(uri_info["uri"])
                if path in references_dict:
                    references_dict[path].append(uri_info["range"])
                else:
                    references_dict[path] = [uri_info["range"]]
                    
            references_counter = 0
            references_content = ""
            for i, (path, ranges) in enumerate(references_dict.items()):
                references_content += "\n" + REFERENCE_PATH + path + REFERENCE_ENDC + "\n"
                
                for range in ranges:
                    with open(path) as f:
                        line = range["start"]["line"]
                        start_column = range["start"]["character"]
                        end_column = range["end"]["character"]
                        line_content = linecache.getline(path, range["start"]["line"] + 1)
                        
                        references_content += "{}:{}:{}".format(
                            line + 1,
                            start_column,
                            line_content[:start_column] + REFERENCE_TEXT + line_content[start_column:end_column] + REFERENCE_ENDC + line_content[end_column:])
                        references_counter += 1
                        
            eval_in_emacs("lsp-bridge-popup-references", [references_content, references_counter])

    def handle_prepare_rename_response(self, request_id, response_result):
        if request_id == self.prepare_rename_request_list[-1]:
            eval_in_emacs("lsp-bridge-rename-highlight", [
                self.filepath,
                response_result["start"]["line"],
                response_result["start"]["character"],
                response_result["end"]["character"]
            ])

    def handle_rename_response(self, request_id, response_result):
        if request_id == self.rename_request_list[-1]:
            if response_result:
                try:
                    counter = 0
                    rename_files = []
                    for rename_info in response_result["documentChanges"]:
                        (rename_file, rename_counter) = self.rename_symbol_in_file(rename_info)
                        rename_files.append(rename_file)
                        counter += rename_counter

                    eval_in_emacs("lsp-bridge-rename-finish", [rename_files, counter])
                except:
                    print("* Failed information about rename response.")
                    import traceback
                    traceback.print_exc()

    def rename_symbol_in_file(self, rename_info):
        rename_file = rename_info["textDocument"]["uri"]
        if rename_file.startswith("file://"):
            rename_file = uri_to_path(rename_file)

        lines = []
        rename_counter = 0

        with open(rename_file, "r") as f:
            lines = f.readlines()

            line_offset_dict = {}

            edits = rename_info["edits"]
            for edit_info in edits:
                # Get replace line.
                replace_line = edit_info["range"]["start"]["line"]

                # Get current line offset, if previous edit is same as current line.
                # We need add changed offset to make sure current edit has right column.
                replace_line_offset = 0
                if replace_line in line_offset_dict:
                    replace_line_offset = line_offset_dict[replace_line]
                else:
                    line_offset_dict[replace_line] = 0

                # Calculate replace column offset.
                replace_column_start = edit_info["range"]["start"]["character"] + replace_line_offset
                replace_column_end = edit_info["range"]["end"]["character"] + replace_line_offset

                # Get current line.
                line_content = lines[replace_line]

                # Get new changed offset.
                new_text = edit_info["newText"]
                replace_offset = len(new_text) - (replace_column_end - replace_column_start)

                # Overlapping new changed offset.
                line_offset_dict[replace_line] = line_offset_dict[replace_line] + replace_offset

                # Replace current line.
                new_line_content = line_content[:replace_column_start] + new_text + line_content[replace_column_end:]
                lines[replace_line] = new_line_content

                rename_counter += 1

        with open(rename_file, "w") as f:
            f.writelines(lines)

        return (rename_file, rename_counter)
