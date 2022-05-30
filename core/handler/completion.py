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

    def process_request(self, position, char) -> dict:
        if char in self.file_action.lsp_server.completion_trigger_characters:
            context = dict(triggerCharacter=char,
                           triggerKind=CompletionTriggerKind.TriggerCharacter.value)
        else:
            context = dict(triggerKind=CompletionTriggerKind.Invoked.value)
        self.position = position
        return dict(position=position, context=context)

    def process_response(self, response: dict) -> None:
        # Get completion items.
        completion_candidates = []

        if response is not None:
            for item in response["items"] if "items" in response else response:
                kind = KIND_MAP[item.get("kind", 0)]
                label = item["label"]

                candidate = {
                    "label": label,
                    "tags": item.get("tags", []),
                    "insertText": item.get('insertText', None),
                    "kind": kind,
                    "annotation": item.get("detail", kind),
                    "insertTextFormat": item.get("insertTextFormat", ''),
                    "textEdit": item.get("textEdit", None)
                }

                if self.file_action.enable_auto_import:
                    candidate["additionalTextEdits"] = item.get("additionalTextEdits", [])

                completion_candidates.append(candidate)
                
                self.file_action.completion_items["{},{}".format(label, kind)] = item

        # Calculate completion common string.
        completion_common_string = os.path.commonprefix(list(map(lambda candidate: candidate["label"], completion_candidates)))
        
        self.file_action.last_completion_candidates = completion_candidates
        
        logger.info("\n--- Completion items number: {}".format(len(completion_candidates)))

        if len(completion_candidates) > 0:
            eval_in_emacs("lsp-bridge-record-completion-items",
                          self.file_action.filepath,
                          completion_common_string,
                          completion_candidates,
                          self.position,
                          self.file_action.lsp_server.server_info["name"],
                          self.file_action.lsp_server.completion_trigger_characters,
                          self.file_action.lsp_server.completion_resolve_provider)
