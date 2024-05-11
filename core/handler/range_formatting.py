from core.handler import Handler
from core.utils import *


class RangeFormatting(Handler):
    name = "rangeFormatting"
    method = "textDocument/rangeFormatting"
    cancel_on_change = True
    provider = "range_format_provider"
    provider_message = "Current server not support range format."

    def process_request(self, range_start, range_end, tab_size) -> dict:
        range = {
            "start": range_start,
            "end": range_end
        }

        options = {
            "tabSize": tab_size,
            "insertSpaces": self.file_action.insert_spaces,
            "trimTrailingWhitespace": True,
            "insertFinalNewline": False,
            "trimFinalNewlines": True,
        }

        return dict(range=range, options=options)

    def process_response(self, response) -> None:
        if response and len(response) > 0:
            eval_in_emacs("lsp-bridge-format--update", self.file_action.filepath, response)
        else:
            message_emacs("Nothing need format.")
