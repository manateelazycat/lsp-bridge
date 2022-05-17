import os
from enum import Enum

from core.handler import Handler
from core.utils import *

KIND_MAP = ["", "Text", "Method", "Function", "Constructor", "Field",
            "Variable", "Class", "Interface", "Module", "Property",
            "Unit", "Value", "Enum", "Keyword", "Snippet", "Color",
            "File", "Reference", "Folder", "EnumMember", "Constant",
            "Struct", "Event", "Operator", "TypeParameter"]


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

        return dict(position=position, context=context)

    def calc_completion_prefix_string(self):
        ret = self.file_action.last_change_file_before_cursor_text
        for c in self.file_action.lsp_server.completion_trigger_characters + [" " + '\t']:
            ret = ret.rpartition(c)[2]
        return ret

    def process_response(self, response: dict) -> None:
        # Calculate completion prefix string.
        completion_prefix_string = self.calc_completion_prefix_string()

        # Get completion items.
        completion_items = []
        completion_candidates = []

        if response is not None:
            for item in response["items"] if "items" in response else response:
                insert_text = item.get("insertText", item["label"]).strip()

                # We need replace prefix string with textEdit character diff if we use insertText as candidate.
                try:
                    if ("insertText" in item and
                            "textEdit" in item and
                            "insertTextFormat" in item and
                            item["insertTextFormat"] == 1):
                        replace_range = item["textEdit"]["range"]["end"]["character"] - \
                                        item["textEdit"]["range"]["start"]["character"]
                        insert_text = insert_text[replace_range:]
                except:
                    pass

                completion_items.append(insert_text)
                kind = KIND_MAP[item.get("kind", 0)]
                candidate = {
                    "label": item["label"],
                    "insertText": insert_text,
                    "kind": kind,
                    "annotation": (item.get("detail") or kind).replace(" ", ""),
                }

                if self.file_action.enable_auto_import:
                    candidate["additionalTextEdits"] = item.get("additionalTextEdits", [])

                completion_candidates.append(candidate)

        # Calculate completion common string.
        completion_common_string = os.path.commonprefix(completion_items)

        # Push completion items to Emacs.
        if (len(completion_items) == 1 and
                completion_prefix_string == completion_common_string == completion_items[0]):
            # Clear completion items if user input last completion item.
            eval_in_emacs("lsp-bridge-record-completion-items",
                          self.file_action.filepath, completion_prefix_string,
                          completion_common_string, [])
        else:
            eval_in_emacs("lsp-bridge-record-completion-items",
                          self.file_action.filepath, completion_prefix_string,
                          completion_common_string, completion_candidates)

