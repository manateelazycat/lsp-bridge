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
            message_emacs(response["signatures"][0]["label"])
