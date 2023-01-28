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
    
    def compare_candidates(self, x, y):
        prefix = self.prefix.lower()
        x_label : str = x["label"].lower()
        y_label : str = y["label"].lower()
        x_include_prefix = x_label.startswith(prefix)
        y_include_prefix = y_label.startswith(prefix)
        
        if x_include_prefix and not y_include_prefix:
            return -1
        elif y_include_prefix and not x_include_prefix:
            return 1
        elif len(x_label) == len(y_label):
            return self.sort_dict[x["key"]] < self.sort_dict[y["key"]]
        else:
            return len(x_label) < len(y_label)
    
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
                
                # If LSP Sever return `isIncomplete` is False, need filter some candidate.
                if filter and not string_match(label.lower(), filter.lower(), fuzzy=fuzzy):
                    continue
                
                annotation = kind if kind != "" else item.get("detail", "")

                # Key use label, don't add index in key, elisp hashmap will create new item when index change.
                key = label
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
            
        log_time("Recv completion candidates number {} from '{}' for file {}".format(
            len(completion_candidates),
            self.method_server_name,
            os.path.basename(self.file_action.filepath)))
        
        # Avoid returning too many items to cause Emacs to do GC operation.
        completion_candidates = completion_candidates[:min(len(completion_candidates), self.file_action.completion_items_limit)]
        
        eval_in_emacs("lsp-bridge-completion--record-items",
                      self.file_action.filepath,
                      completion_candidates,
                      self.position,
                      self.method_server_name,
                      self.method_server.completion_trigger_characters,
                      self.file_action.get_lsp_server_names())
