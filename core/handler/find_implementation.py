from core.handler import Handler
from core.utils import *


class FindImplementation(Handler):
    name = "find_implementation"
    method = "textDocument/implementation"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        if not response:
            message_emacs("No implementation found.")
            return

        file_info = response[0]
        # volar return only LocationLink (using targetUri)
        file_uri = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
        filepath = Path(uri=file_uri)
        range1 = file_info["range"] if "range" in file_info else file_info["targetRange"]
        startpos = range1["start"]
        eval_in_emacs("lsp-bridge--jump-to-def", filepath, startpos)
