from core.handler import Handler
from core.utils import *


class SignatureHelp(Handler):
    name = "signature_help"
    method = "textDocument/signatureHelp"
    cancel_on_change = True
    provider = "signature_help_provider"

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        if response is not None:
            if len(response["signatures"]) > 0 and "parameters" in response["signatures"][0]:
                eval_in_emacs("lsp-bridge-signature-help-update",
                              list(map(lambda p: p["label"].split(":")[0], response["signatures"][response.get("activeSignature", 0)]["parameters"])),
                              response.get("activeParameter", 0))
