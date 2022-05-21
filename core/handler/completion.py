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
        # We search trigger chars backward to find bound of candidate prefix, and works well in most LSP servers,
        # but not work for some LSP server, such as Volar, 
        # when `prefixBoundChars` setting in server.json, we only search blank to find bound of candidate prefix.
        if "prefixBoundChars" in self.file_action.lsp_server.server_info:
            search_characters = self.file_action.lsp_server.server_info["prefixBoundChars"]
        else:
            search_characters = self.file_action.lsp_server.completion_trigger_characters + [" ", "\t"]
        
        trigger_characters_index = list(filter(lambda res: res != -1,
                                               list(map(lambda char: self.file_action.last_change_file_before_cursor_text.rfind(char), 
                                                        search_characters))))
        
        if len(trigger_characters_index) > 0:
            prefix_string = self.file_action.last_change_file_before_cursor_text[max(trigger_characters_index) + 1:]
            
            # We need strim first trigger char if option `prefixBoundChars` is enable.
            if "prefixBoundChars" in self.file_action.lsp_server.server_info:
                if len(prefix_string) > 0 and prefix_string[0] in self.file_action.lsp_server.completion_trigger_characters:
                    prefix_string = prefix_string[1:]
            
            return prefix_string
        else:
            return self.file_action.last_change_file_before_cursor_text

    def process_response(self, response: dict) -> None:
        # Calculate completion prefix string.
        completion_prefix_string = self.calc_completion_prefix_string()

        # Get completion items.
        completion_candidates = []

        if response is not None:
            for item in response["items"] if "items" in response else response:
                kind = KIND_MAP[item.get("kind", 0)]
                
                candidate = {
                    "label": item["label"],
                    "tags": item.get("tags", []),
                    "insertText": item.get('insertText', None),
                    "kind": kind,
                    "annotation": (item.get("detail") or kind).replace(" ", ""),
                    "insertTextFormat": item.get("insertTextFormat", ''),
                    "textEdit": item.get("textEdit", None)
                }

                if self.file_action.enable_auto_import:
                    candidate["additionalTextEdits"] = item.get("additionalTextEdits", [])

                completion_candidates.append(candidate)

        # Calculate completion common string.
        completion_common_string = os.path.commonprefix(list(map(lambda candidate: candidate["label"], completion_candidates)))
        
        # Push completion items to Emacs.
        if (len(completion_candidates) == 1 and completion_prefix_string == completion_common_string == completion_candidates[0]["label"]):
            # Clear completion items if user input last completion item.
            eval_in_emacs("lsp-bridge-record-completion-items",
                          self.file_action.filepath, completion_prefix_string, completion_common_string, [])
        else:
            eval_in_emacs("lsp-bridge-record-completion-items", 
                          self.file_action.filepath, completion_prefix_string, completion_common_string, completion_candidates)
