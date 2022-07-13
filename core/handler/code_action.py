from core.handler import Handler
from core.utils import *


class CodeAction(Handler):
    name = "code_action"
    method = "textDocument/codeAction"
    cancel_on_change = True
    provider = "code_action_provider"
    provider_message = "Current server not support code action."

    def process_request(self, range_start, range_end, action_kind) -> dict:
        self.action_kind = action_kind
        
        range = {
            "start": range_start,
            "end": range_end
        }
        
        match_diagnostic = []
        diagnostics = self.file_action.diagnostics
        for diagnostic in diagnostics:
            if (range["start"]["line"] >= diagnostic["range"]["start"]["line"] and 
                range["start"]["character"] >= diagnostic["range"]["start"]["character"] and 
                range["end"]["line"] <= diagnostic["range"]["end"]["line"] and 
                range["end"]["character"] <= diagnostic["range"]["end"]["character"]):
                match_diagnostic.append(diagnostic)
        
        if isinstance(action_kind, str):
            context = {
                "diagnostics": match_diagnostic,
                "only": [action_kind]
            }
        else:
            context = {
                "diagnostics": match_diagnostic
            }
        
        return dict(range=range, context=context)

    def process_response(self, response) -> None:
        if response != None and len(response) > 0:
            self.file_action.code_action_response = response    # type: ignore
            eval_in_emacs("lsp-bridge-code-action-fix", response, self.action_kind)
        else:
            self.file_action.code_action_response = None
            message_emacs("No code action found.")
