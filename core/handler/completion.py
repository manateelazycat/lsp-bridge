import re
import os
from enum import Enum
from functools import cmp_to_key

from core.handler import Handler
from core.utils import *


class CompletionTriggerKind(Enum):
    Invoked = 1
    TriggerCharacter = 2
    TriggerForIncompleteCompletions = 3


class Completion(Handler):
    name = "completion"
    method = "textDocument/completion"
    cancel_on_change = True

    def process_request(self, lsp_server, position, char, prefix, version) -> dict:
        self.method_server = lsp_server
        self.method_server_name = self.method_server.server_info["name"]

        if char in self.method_server.completion_trigger_characters:
            context = dict(triggerCharacter=char,
                           triggerKind=CompletionTriggerKind.TriggerCharacter.value)
        else:
            context = dict(triggerKind=CompletionTriggerKind.Invoked.value)
        self.position = position
        self.prefix = prefix
        self.version = version
        return dict(position=position, context=context)

    def parse_sort_value(self, sort_text):
        return ''.join(c for c in sort_text if c.isdigit() or c == '.').rstrip('.')

    def compare_candidates(self, x, y):
        prefix = self.prefix.lower()
        x_label, y_label = (x["filterText"] or x["label"]).lower(), (y["filterText"] or y["label"]).lower()
        x_icon, y_icon = x["icon"], y["icon"]
        x_score, y_score = x["score"], y["score"]
        x_sort_text, y_sort_text = map(self.parse_sort_value, (x["sortText"], y["sortText"]))
        x_include_prefix, y_include_prefix = x_label.startswith(prefix), y_label.startswith(prefix)
        x_method_name, y_method_name = x_label.split('(')[0], y_label.split('(')[0]

        # Sort by prefix.
        # If the number of LSP server return candidates is much bigger than acm-backend-lsp-candidates-max-number limit.
        # We need sort prefix first even acm-backend-lsp-match-mode is fuzzy.
        # Otherwise, the front part of the candidates cannot be matched user input prefix.
        if x_include_prefix != y_include_prefix:
            return -1 if x_include_prefix else 1

        # Sort file by score, score is provided by LSP server.
        if x_score != y_score:
            return 1 if x_score < y_score else -1

        # Sort file by sortText, sortText is provided by LSP server.
        if x_sort_text and y_sort_text and x_sort_text != y_sort_text:
            return -1 if x_sort_text < y_sort_text else 1

        # Sort by method name if both candidates are method.
        if x_icon == y_icon == "method" and x_method_name != y_method_name:
            return -1 if x_method_name < y_method_name else 1

        # Sort by length.
        return -1 if len(x_label) < len(y_label) else (1 if len(x_label) > len(y_label) else 0)

    def get_display_new_text(self):
        for server in self.file_action.get_match_lsp_servers("completion"):
            if server.server_info.get("displayNewText", False):
                return True

        return False

    def get_display_label(self, item, display_new_text):
        label = item["label"]
        detail = item.get("detail", "")

        # Get display label.
        if isinstance(detail, str) and detail.strip() != "":
            detail_label = f"{label} => {detail}"
        else:
            detail_label = label

        # Optimizing for Rust
        try:
            if (self.method_server_name == "rust-analyzer") and ("(\u2026)" in label or "()" in label): # '\u2026' is the unicode char: '…'
                function_name = label.split('(')[0]

                match_fn = re.search(r'\bfn\b', function_name)
                if match_fn:
                    # When finding an ellipsis in 'label'
                    # replace 'fn' with function name in 'label'
                    detail_label = re.sub(r'\bfn\b', function_name, detail)

                    # Remove `pub` or `pub const` before function name.
                    match = re.search(r'\b(\w+\s*\([^)]*\)\s*->\s*\w+)\b', detail_label)
                    if match:
                        # If a match is successful, pick up the captured group
                        detail_label = match.group(1)
                else:
                    detail_label = function_name
        except:
            pass

        # Avoid label length not longer than display_label_max_length.
        if len(detail_label) > self.file_action.display_label_max_length:
            display_label = detail_label[:self.file_action.display_label_max_length] + " ..."
        else:
            display_label = detail_label

        # displayNewText use for Pin Yin LSP server.
        if display_new_text:
            text_edit = item.get("textEdit", None)
            if text_edit is not None:
                display_label = text_edit.get("newText", None)

        return display_label

    def fnv_1a(self, data):
        '''
        FNV-1a is a non-cryptographic hash function, faster than md5.
        '''
        h = 2166136261
        for byte in data:
            h = (h ^ byte) * 16777219
        return h

    def convert_snippet(self, snippet):
        '''
        Convert LSP snippet to YASnippet.
        LSP snippet is ALMOST a valid yas snippet, but the server might returns
            ${1:Placeholder} ... ${1:Placeholder}
        which is not supported. Mirrors can't have placeholder themselves:
            ${1:Placeholder} ... ${1}
        https://joaotavora.github.io/yasnippet/snippet-development.html#org087775c
        '''
        placeholder_regex = r'\$\{(\d+):([^}]+)\}'
        placeholders = {}

        def replace(match):
           index, name = match.groups()
           if name not in placeholders:
               placeholders[name] = index
               return match.group(0)
           else:
               return f"${{{placeholders[name]}}}"

        return re.sub(placeholder_regex, replace, snippet)

    def process_response(self, response: dict) -> None:
        # Get completion items.
        completion_candidates = []
        items = {}

        if response is not None:
            # Get match mode and case mode to filter candidates.
            match_mode = self.file_action.completion_match_mode
            case_mode = self.file_action.completion_case_mode

            # Some LSP server, such as Wen, need assign textEdit/newText to displayLabel.
            display_new_text = self.get_display_new_text()

            for item in response["items"] if "items" in response else response:
                kind = KIND_MAP[item.get("kind", 0)].lower()
                label = item["label"]
                detail = item.get("detail", "")

                # Filter candidate that kind match acm-backend-lsp-block-kind-list.
                try:
                    if self.file_action.completion_block_kind_list is not False and kind in self.file_action.completion_block_kind_list:
                        continue
                except:
                    pass

                # The lsp-bridge will continuously filter candidates on the Python side.
                # If not filter and the value of `acm-backend-lsp-candidates-max-number' is far smaller
                # than the number of candidates returned by the LSP server,
                # it will cause the lsp-bridge to always send the previous batch of candidates
                # which do not match the users input.
                if not string_match(label, self.prefix, match_mode, case_mode):
                    continue

                annotation = kind if kind != "" else detail

                # The key keyword combines the values ​​of 'label' and 'detail'
                # to handle different libraries provide the same function.
                key = f"{label}_{detail}"

                if self.file_action.enable_auto_import:
                    # For imports, the "detail" can often be the same for all symbols.
                    # Make the key unique.

                    key += "_" + "_".join(
                        format(self.fnv_1a(x["newText"].encode('utf-8')), 'x')[:8]
                        for x in item.get("additionalTextEdits", []))

                insert_text = item.get('insertText', None)
                text_edit = item.get("textEdit", None)

                if kind == "snippet":
                    if text_edit is not None:
                        text_edit["newText"] = self.convert_snippet(text_edit["newText"])
                    elif insert_text is not None:
                        insert_text = self.convert_snippet(insert_text)
                    else:
                        label = self.convert_snippet(label)

                # Build candidate.
                candidate = {
                    "key": key,
                    "icon": annotation,
                    "label": label,
                    "displayLabel": self.get_display_label(item, display_new_text),
                    "deprecated": 1 in item.get("tags", []),
                    "insertText": insert_text,
                    "insertTextFormat": item.get("insertTextFormat", ''),
                    "textEdit": text_edit,
                    "score": item.get("score", 1000),
                    "sortText": item.get("sortText", ""),
                    "filterText": item.get("filterText", None),
                    "server": self.method_server_name,
                    "backend": "lsp"
                }

                if self.file_action.enable_auto_import:
                    candidate["additionalTextEdits"] = item.get("additionalTextEdits", [])

                completion_candidates.append(candidate)

                items[key] = item

            self.file_action.completion_items[self.method_server_name] = items

            completion_candidates = sorted(completion_candidates, key=cmp_to_key(self.compare_candidates))

        log_time("Got completion candidates ({}) from '{}' for file {}".format(
            len(completion_candidates),
            self.method_server_name,
            os.path.basename(self.file_action.filepath)))

        # Avoid returning too many items to cause Emacs to do GC operation.
        completion_candidates = completion_candidates[:min(len(completion_candidates), self.file_action.completion_items_limit)]

        # Just call lsp-bridge-completion--record-items method when 'version' is newest version of file action.
        if self.version == self.file_action.version:
            log_time("Record completion candidates ({}) from '{}' for file {}".format(
                len(completion_candidates),
                self.method_server_name,
                os.path.basename(self.file_action.filepath)))

            eval_in_emacs("lsp-bridge-completion--record-items",
                          self.file_action.filepath,
                          get_lsp_file_host(),
                          completion_candidates,
                          self.position,
                          self.method_server_name,
                          self.method_server.completion_trigger_characters,
                          self.file_action.get_lsp_server_names())
