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

    def process_request(self, lsp_server, position, char, prefix) -> dict:
        self.method_server = lsp_server
        self.method_server_name = self.method_server.server_info["name"]
        
        if char in self.method_server.completion_trigger_characters:
            context = dict(triggerCharacter=char,
                           triggerKind=CompletionTriggerKind.TriggerCharacter.value)
        else:
            context = dict(triggerKind=CompletionTriggerKind.Invoked.value)
        self.position = position
        self.prefix = prefix
        return dict(position=position, context=context)

    def parse_sort_value(self, sort_text):
        if sort_text == "":
            return sort_text
        else:
            sort_text = ''.join(c for c in sort_text if c.isdigit() or c == '.')

            if sort_text.endswith("."):
                sort_text = sort_text[:-1]

            return sort_text
    
    def compare_candidates(self, x, y):
        prefix = self.prefix.lower()
        x_label : str = x["label"].lower()
        y_label : str = y["label"].lower()
        x_icon : str = x["icon"]
        y_icon : str = y["icon"]
        x_sort_text : str = self.parse_sort_value(x["sortText"])
        y_sort_text : str = self.parse_sort_value(y["sortText"])
        x_include_prefix = x_label.startswith(prefix)
        y_include_prefix = y_label.startswith(prefix)
        x_method_name = x_label.split('(')[0]
        y_method_name = y_label.split('(')[0]

        # 1. Sort file by sortText, sortText is provided by LSP server.
        if x_sort_text != "" and y_sort_text != "" and x_sort_text != y_sort_text:
            if x_sort_text < y_sort_text:
                return -1
            elif x_sort_text > y_sort_text:
                return 1
        # 2. Sort by prefix.
        elif x_include_prefix and not y_include_prefix:
            return -1
        elif y_include_prefix and not x_include_prefix:
            return 1
        # 3. Sort by method name if both candidates are method.
        elif x_icon == "method" and y_icon == "method" and x_method_name != y_method_name:
            if x_method_name < y_method_name:
                return -1
            elif x_method_name > y_method_name:
                return 1
        # 4. Sort by length.
        elif len(x_label) < len(y_label):
            return -1
        elif len(x_label) > len(y_label):
            return 1
        else:
            return 0
    
    def process_response(self, response: dict) -> None:
        # Get completion items.
        completion_candidates = []
        self.sort_dict = {}
        items = {}

        if response is not None:
            item_index = 0
            
            try:
                # If LSP Sever return `isIncomplete` is False, we need pick value of option `incomplete-fuzzy-match`.
                filter = self.prefix if not response.get("isIncomplete") else None
            except:
                filter = None
                
            fuzzy = False
            if filter:
                for server in self.file_action.get_match_lsp_servers("completion"):
                    if server.server_name.endswith("#" + self.method_server_name):
                        fuzzy = server.server_info.get("incomplete-fuzzy-match")
                        break
                    
            # Some LSP server, such as Wen, need assign textEdit/newText to display-label.
            display_new_text = False
            for server in self.file_action.get_match_lsp_servers("completion"):
                if server.server_info.get("displayNewText", False):
                    display_new_text = True
                    break
            
            for item in response["items"] if "items" in response else response:
                kind = KIND_MAP[item.get("kind", 0)].lower()
                label = item["label"]
                detail = item.get("detail", "")
                
                # If LSP Sever return `isIncomplete` is False, need filter some candidate.
                if filter and not string_match(label.lower(), filter.lower(), fuzzy=fuzzy):
                    continue
                
                annotation = kind if kind != "" else detail

                # The key keyword combines the values ​​of 'label' and 'detail'
                # to handle different libraries provide the same function.
                key = f"{label}_{detail}"
                display_label = label[:self.file_action.display_label_max_length] + " ..." if len(label) > self.file_action.display_label_max_length else label

                if display_new_text:
                    text_edit = item.get("textEdit", None)
                    if text_edit is not None:
                        display_label = text_edit.get("newText", None)

                candidate = {
                    "key": key,
                    "icon": annotation,
                    "label": label,
                    "display-label": display_label,
                    "deprecated": 1 in item.get("tags", []),
                    "insertText": item.get('insertText', None),
                    "insertTextFormat": item.get("insertTextFormat", ''),
                    "textEdit": item.get("textEdit", None),
                    "sortText": item.get("sortText", ""),
                    "server": self.method_server_name,
                    "backend": "lsp"
                }

                self.sort_dict[key] = item.get("sortText", "")

                if self.file_action.enable_auto_import:
                    candidate["additionalTextEdits"] = item.get("additionalTextEdits", [])

                completion_candidates.append(candidate)

                items[key] = item

                item_index += 1

            self.file_action.completion_items[self.method_server_name] = items

            completion_candidates = sorted(completion_candidates, key=cmp_to_key(self.compare_candidates))

        log_time("Got completion candidates ({}) from '{}' for file {}".format(
            len(completion_candidates),
            self.method_server_name,
            os.path.basename(self.file_action.filepath)))

        # Avoid returning too many items to cause Emacs to do GC operation.
        completion_candidates = completion_candidates[:min(len(completion_candidates), self.file_action.completion_items_limit)]

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
