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

from core.utils import *

KIND_MAP = ["", "Text", "Method", "Function", "Constructor", "Field",
            "Variable", "Class", "Interface", "Module", "Property",
            "Unit" , "Value" , "Enum", "Keyword" , "Snippet", "Color",
            "File", "Reference", "Folder", "EnumMember", "Constant",
            "Struct", "Event", "Operator", "TypeParameter"]

REFERENCE_PATH = '\033[95m'
REFERENCE_TEXT = '\033[94m'
REFERENCE_ENDC = '\033[0m'

class FileAction(object):

    def __init__(self, filepath, project_path, lang_server):
        object.__init__(self)

        # Build request functions.
        for name in ["find_define", "find_references", "prepare_rename", "rename", "completion", "hover"]:
            self.build_request_function(name)

        # Init.
        self.filepath = filepath
        self.project_path = project_path
        self.request_dict = {}
        self.last_change_file_time = -1
        self.last_change_file_before_cursor_text = ""
        self.last_change_cursor_time = -1
        self.completion_prefix_string = ""
        self.version = 1
        self.try_completion_timer = None

        # Read language server information.
        self.lang_server_info = None
        self.load_lang_server_info(lang_server)

        self.lsp_server = None

        # Generate initialize request id.
        self.initialize_id = generate_request_id()
        self.enable_auto_import = get_emacs_var("lsp-bridge-enable-auto-import")

    def load_lang_server_info(self, lang_server):
        lang_server_info_path = ""
        if os.path.exists(lang_server) and os.path.sep in lang_server:
            # If lang_server is real file path, we load the LSP server configuration from the user specified file.
            lang_server_info_path = lang_server
        else:
            # Otherwise, we load LSP server configuration from file lsp-bridge/langserver/lang_server.json.
            lang_server_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), "langserver")
            lang_server_file_path_current = os.path.join(lang_server_dir, "{}_{}.json".format(lang_server, os.name))
            lang_server_file_path_default = os.path.join(lang_server_dir, "{}.json".format(lang_server))

            lang_server_info_path = lang_server_file_path_current if os.path.exists(lang_server_file_path_current) else lang_server_file_path_default

        with open(lang_server_info_path) as f:
            import json
            self.lang_server_info = json.load(f)

    def get_lsp_server_name(self):
        # We use project path and LSP server type as unique name.
        return "{}#{}".format(path_as_key(self.project_path), self.lang_server_info["name"])

    def change_file(self, start, end, range_length, change_text, position, before_char, before_cursor_text):
        # Send didChange request to LSP server.
        if self.lsp_server is not None:
            self.lsp_server.send_did_change_notification(
                self.filepath, self.version, start, end, range_length, change_text)
        else:
            # Please report bug if you got this message.
            logger.info("IMPOSSIBLE HERE: change_file %s %s", self.filepath, self.lsp_server)

        self.version += 1

        # Try cancel expired completion timer.
        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        # Record last change information.
        self.last_change_file_time = time.time()
        self.last_change_file_before_cursor_text = before_cursor_text

        # Send textDocument/completion 100ms later.
        self.try_completion_timer = threading.Timer(0.1, lambda : self.completion(position, before_char))
        self.try_completion_timer.start()

    def change_cursor(self):
        # Record change cursor time.
        self.last_change_cursor_time = time.time()

    def save_file(self):
        self.lsp_server.send_did_save_notification(self.filepath)

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

        # Init request list variable.
        setattr(self, "{}_request_list".format(name), [])

        # Init request method.
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
        elif request_type == "hover":
            self.handle_hover_response(request_id, response_result)

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
            completion_candidates = []

            if response_result is not None:
                for item in response_result["items"] if "items" in response_result else response_result:
                    completion_items.append(item["insertText"] if "insertText" in item else item["label"])
                    kind = KIND_MAP[item.get("kind", 0)]
                    candidate = {
                        "label": completion_items[-1],
                        "kind": kind,
                        #  HACK: space makes the number of args for emacs wrong
                        "annotation": item.get("detail", kind).replace(" ", ""),
                        #  TODO: the situtation for documentation is complex
                        # "documents": str(item.get("documentation", ""))
                    }
                    if (self.enable_auto_import):
                        candidate["additionalTextEdits"] = item.get("additionalTextEdits", [])
                    completion_candidates.append(candidate)

            # Calcuate completion common string.
            completion_common_string = os.path.commonprefix(completion_items)

            # Push completion items to Emacs.
            if len(completion_items) == 1 and (self.completion_prefix_string == completion_common_string == completion_items[0]):
                # Clear completion items if user input last completion item.
                eval_in_emacs("lsp-bridge-record-completion-items", [self.filepath, self.completion_prefix_string,
                                                                     completion_common_string, []])
            else:
                eval_in_emacs("lsp-bridge-record-completion-items", [self.filepath, self.completion_prefix_string,
                                                                     completion_common_string, completion_candidates])

    def calc_completion_prefix_string(self):
        ret = self.last_change_file_before_cursor_text
        for c in self.lsp_server.completion_trigger_characters + [" " + '\t']:
            ret = ret.rpartition(c)[2]
        return ret

    def handle_find_define_response(self, request_id, response_result):
        # Stop send jump define if request id expired, or change file, or move cursor.
        if (request_id == self.find_define_request_list[-1] and
            self.request_dict[request_id]["last_change_file_time"] == self.last_change_file_time and
            self.request_dict[request_id]["last_change_cursor_time"] == self.last_change_cursor_time):

            if response_result:
                try:
                    file_info = response_result[0]
                    # volar return only LocationLink (using targetUri)
                    fileuri = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
                    filepath = uri_to_path(fileuri)
                    range1 = file_info["range"] if "range" in file_info else file_info["targetRange"]
                    startpos = range1["start"]
                    row = startpos["line"]
                    column = startpos["character"]
                    eval_in_emacs("lsp-bridge--jump-to-def", [filepath, row, column])
                except:
                    logger.info("* Failed information about find_define response.")
                    import traceback
                    traceback.print_exc()
            else:
                eval_in_emacs("message", ["[LSP-Bridge] Can't find define."])

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

                    for rename_info in response_result["documentChanges"] if "documentChanges" in response_result else response_result["changes"]:
                        (rename_file, rename_counter) = self.rename_symbol_in_file(rename_info)
                        rename_files.append(rename_file)
                        counter += rename_counter

                    eval_in_emacs("lsp-bridge-rename-finish", [rename_files, counter])
                except:
                    logger.info("* Failed information about rename response.")
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

    def handle_hover_response(self, request_id, response_result):
        import linecache

        if request_id == self.hover_request_list[-1]:
            if (response_result is not None and
                "range" in response_result and
                "contents" in response_result):
                line = response_result["range"]["start"]["line"]
                start_column = response_result["range"]["start"]["character"]
                end_column = response_result["range"]["end"]["character"]

                line_content = linecache.getline(self.filepath, line + 1)
                contents = response_result["contents"]
                render_string = self.parse_hover_contents(contents, [])

                eval_in_emacs("lsp-bridge-popup-documentation", ["",
                                                                 line_content[start_column:end_column],
                                                                 render_string])
            else:
                eval_in_emacs("message", ["[LSP-Bridge] No documentation here."])

    def parse_hover_contents(self, contents, render_strings):
        content_type = type(contents)
        if content_type == str:
            render_strings.append(self.make_code_block("text", contents))
        elif content_type == dict:
            if "kind" in contents:
                if contents["kind"] == "markdown":
                    render_strings.append(contents["value"])
                else:
                    render_strings.append(self.make_code_block(self.lang_server_info["languageId"], contents["value"]))
            elif "language" in contents:
                render_strings.append(self.make_code_block(contents["language"], contents["value"]))
        elif content_type == list:
            for item in contents:
                if item != "":
                    self.parse_hover_contents(item, render_strings)
        return "\n".join(render_strings)

    def make_code_block(self, language, string):
        return "```{language}\n{string}\n```".format(language=language, string=string)
