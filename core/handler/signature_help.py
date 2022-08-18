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
                parameters = response["signatures"][response.get("activeSignature", 0)]["parameters"]
                
                arguments = []
                active_argument_index = response.get("activeParameter", 0)
                for index, parameter in enumerate(parameters):
                    label = parameter["label"]
                    if isinstance(label, str): # most lsp server return string
                        argument = label if index == active_argument_index else label.split(":")[0]
                        arguments.append(argument)
                    elif isinstance(label, list) and len(label) == 2: # ccls return list
                        signatures_label = response["signatures"][0]["label"]
                        arguments.append(signatures_label[label[0]:label[1]])
                        
                eval_in_emacs("lsp-bridge-signature-help-update", arguments, response.get("activeParameter", 0))
