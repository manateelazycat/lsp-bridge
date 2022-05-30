import os
from enum import Enum

from core.handler import Handler
from core.utils import *

class CompletionItem(Handler):
    name = "completion_item_resolve"
    method = "completionItem/resolve"
    cancel_on_change = True
    send_document_uri = False

    def process_request(self, item) -> dict:
        return item

    def process_response(self, response: dict) -> None:
        if response is not None and "documentation" in response and response["documentation"]["value"] != "":
            eval_in_emacs("lsp-bridge-popup-completion-item-doc", self.file_action.filepath, response["documentation"]["value"])
