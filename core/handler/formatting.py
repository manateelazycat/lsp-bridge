from core.handler import Handler
from core.utils import *


class Formatting(Handler):
    name = "formatting"
    method = "textDocument/formatting"
    cancel_on_change = True
    provider = "code_format_provider"
    provider_message = "Current server not support code format."

    def process_request(self, tab_size) -> dict:
        options = {
            "tabSize": tab_size,
            "insertSpaces": self.file_action.insert_spaces,
            "trimTrailingWhitespace": True,
            "insertFinalNewline": False,
            "trimFinalNewlines": True,
        }

        return dict(options=options)

    def process_response(self, response) -> None:
        if response and len(response) > 0:
            remote_connection_info = get_remote_connection_info()
            tramp_path = remote_connection_info + self.file_action.filepath
            eval_in_emacs("lsp-bridge-format--update", tramp_path, response)
        else:
            message_emacs("Nothing need format.")
