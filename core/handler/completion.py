import os
from enum import Enum

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

    def process_request(self, lsp_server, position, char) -> dict:
        self.method_server = lsp_server
        self.method_server_name = self.method_server.server_info["name"]
        
        if char in self.method_server.completion_trigger_characters:
            context = dict(triggerCharacter=char,
                           triggerKind=CompletionTriggerKind.TriggerCharacter.value)
        else:
            context = dict(triggerKind=CompletionTriggerKind.Invoked.value)
        self.position = position
        return dict(position=position, context=context)

    def process_response(self, response: dict) -> None:
        # Get completion items.
        completion_candidates = []
        sort_dict = {}
        items = {}

        if response is not None:
            item_index = 0
            
            for item in response["items"] if "items" in response else response:
                kind = KIND_MAP[item.get("kind", 0)].lower()
                label = item["label"]
                annotation = kind if kind != "" else item.get("detail", "")
                key = "{},{}".format(item_index, label)

                candidate = {
                    "key": key,
                    "icon": annotation,
                    "label": label,
                    "deprecated": 1 in item.get("tags", []),
                    "insertText": item.get('insertText', None),
                    "insertTextFormat": item.get("insertTextFormat", ''),
                    "textEdit": item.get("textEdit", None),
                    "server": self.method_server_name
                }
                
                sort_dict[key] = item.get("sortText", "")

                if self.file_action.enable_auto_import:
                    candidate["additionalTextEdits"] = item.get("additionalTextEdits", [])

                completion_candidates.append(candidate)
                
                items[key] = item
                
                
                item_index += 1
                
            self.file_action.completion_items[self.method_server_name] = items
                
            completion_candidates = sorted(completion_candidates, key=lambda candidate: sort_dict[candidate["key"]])
            
        # Avoid returning too many items to cause Emacs to do GC operation.
        completion_candidates = completion_candidates[:min(len(completion_candidates), self.file_action.completion_items_limit)]
        
        self.file_action.last_completion_candidates[self.method_server_name] = completion_candidates
        
        logger.info("\n--- Completion items number: {}".format(len(completion_candidates)))
        
        if len(completion_candidates) > 0:
            eval_in_emacs("lsp-bridge-record-completion-items",
                          self.file_action.filepath,
                          completion_candidates,
                          self.position,
                          self.method_server_name,
                          self.method_server.completion_trigger_characters,
                          self.file_action.get_lsp_server_names())
