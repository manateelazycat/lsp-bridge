from core.handler import Handler
from core.utils import *


class PrepareRename(Handler):
    name = "prepare_rename"
    method = "textDocument/prepareRename"
    provider = "rename_prepare_provider"

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        # compatible for gopls
        if "range" in response:
            response = response["range"]

        eval_in_emacs("lsp-bridge-rename-highlight", self.file_action.filepath, response["start"], response["end"])
