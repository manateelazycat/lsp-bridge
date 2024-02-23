import os
import hashlib
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
        x_label, y_label = x["label"].lower(), y["label"].lower()
        x_icon, y_icon = x["icon"], y["icon"]
        x_score, y_score = x["score"], y["score"]
        x_sort_text, y_sort_text = map(self.parse_sort_value, (x["sortText"], y["sortText"]))
        x_include_prefix, y_include_prefix = x_label.startswith(prefix), y_label.startswith(prefix)
        x_method_name, y_method_name = x_label.split('(')[0], y_label.split('(')[0]

        # Sort file by score, score is provided by LSP server.
        if x_score != y_score:
            return 1 if x_score < y_score else -1

        # Sort file by sortText, sortText is provided by LSP server.
        if x_sort_text and y_sort_text and x_sort_text != y_sort_text:
            return -1 if x_sort_text < y_sort_text else 1

        # Sort by prefix.
        if x_include_prefix != y_include_prefix:
            return -1 if x_include_prefix else 1

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
        if detail.strip() != "":
            detail_label = f"{label} => {detail}"
        else:
            detail_label = label

        try:
            if "\u2026" in label and "(" in label:
                # Optimizing for Rust
                # When finding an ellipsis in 'label'
                # replace 'fn' with function name in 'label'
                function_name = label.split('(')[0]
                detail_label = re.sub(r'\bfn\b', function_name, detail)
        except:
            pass

        if len(detail_label) > self.file_action.display_label_max_length:
            display_label = detail_label[:self.file_action.display_label_max_length] + " ..."
        else:
            display_label = detail_label

        if display_new_text:
            text_edit = item.get("textEdit", None)
            if text_edit is not None:
                display_label = text_edit.get("newText", None)

        return display_label

    def process_response(self, response: dict) -> None:
        # Get completion items.
        completion_candidates = []
        items = {}

        if response is not None:
            # Get match mode to filter candidates.
            match_mode = self.file_action.completion_match_mode

            # Some LSP server, such as Wen, need assign textEdit/newText to displayLabel.
            display_new_text = self.get_display_new_text()

            for item in response["items"] if "items" in response else response:
                kind = KIND_MAP[item.get("kind", 0)].lower()
                label = item["label"]
                detail = item.get("detail", "")

                # Try to drop current candidate if it match user rule.
                if match_mode == "prefix":
                    if not string_match(label.lower(), self.prefix.lower(), fuzzy=False):
                        continue
                elif match_mode == "prefixCaseSensitive":
                    if not string_match(label, self.prefix, fuzzy=False):
                        continue
                elif match_mode == "fuzzy":
                    if not string_match(label.lower(), self.prefix.lower(), fuzzy=True):
                        continue

                annotation = kind if kind != "" else detail


                # The key keyword combines the values ​​of 'label' and 'detail'
                # to handle different libraries provide the same function.
                key = f"{label}_{detail}"

                if self.file_action.enable_auto_import:
                    # For imports, the "detail" can often be the same for all symbols.
                    # Make the key unique.
                    key += "_" + "_".join(
                        hashlib.md5(x["newText"].encode('utf-8')).hexdigest()[:8]
                        for x in item.get("additionalTextEdits", []))

                # Build candidate.
                candidate = {
                    "key": key,
                    "icon": annotation,
                    "label": label,
                    "displayLabel": self.get_display_label(item, display_new_text),
                    "deprecated": 1 in item.get("tags", []),
                    "insertText": item.get('insertText', None),
                    "insertTextFormat": item.get("insertTextFormat", ''),
                    "textEdit": item.get("textEdit", None),
                    "score": item.get("score", 1000),
                    "sortText": item.get("sortText", ""),
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
