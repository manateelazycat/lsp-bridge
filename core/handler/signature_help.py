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
            eval_in_emacs("lsp-bridge-signature-help-update", 
                          list(map(lambda p: p["label"].split(":")[0], response["signatures"][response["activeSignature"]]["parameters"])),
                          response["activeParameter"])
