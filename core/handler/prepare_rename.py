from core.handler import Handler
from core.utils import *


class PrepareRename(Handler):
    name = "prepare_rename"
    method = "textDocument/prepareRename"

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        eval_in_emacs(
            "lsp-bridge-rename-highlight",
            self.file_action.filepath,
            response["start"]["line"],
            response["start"]["character"],
            response["end"]["character"]
        )
