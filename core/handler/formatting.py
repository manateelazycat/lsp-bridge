from core.handler import Handler
from core.utils import *


class Formatting(Handler):
    name = "formatting"
    method = "textDocument/formatting"
    cancel_on_change = True

    def process_request(self) -> dict:
        options = {
            "tabSize": 4,
            "insertSpaces": True,
            "trimTrailingWhitespace": True,
            "insertFinalNewline": False,
            "trimFinalNewlines": True,
        }
        
        return dict(options=options)

    def process_response(self, response) -> None:
        if response and len(response) > 0:
            eval_in_emacs("lsp-bridge-code-format-fix", self.file_action.filepath, response)
        
