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
        
        diagnostics = self.file_action.diagnostics
                
        if isinstance(action_kind, str):
            context = {
                "diagnostics": diagnostics,
                "only": [action_kind]
            }
        else:
            context = {
                "diagnostics": diagnostics
            }
            
        return dict(range=range, context=context)

    def process_response(self, response) -> None:
        if response != None and len(response) > 0:
            self.file_action.code_action_response = response    # type: ignore
            eval_in_emacs("lsp-bridge-code-action-fix", response, self.action_kind)
        else:
            self.file_action.code_action_response = None
            message_emacs("No code action found.")
