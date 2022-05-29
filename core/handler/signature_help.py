from core.handler import Handler
from core.utils import *


class SignatureHelp(Handler):
    name = "signature_help"
    method = "textDocument/signatureHelp"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        if response is not None:
            if len(response["signatures"]) > 0:
                eval_in_emacs("lsp-bridge-show-signature-help", response["signatures"][0]["label"])
