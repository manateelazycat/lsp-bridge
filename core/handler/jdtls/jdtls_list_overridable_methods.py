from core.handler import Handler
from core.utils import *
import json

class JdtlsListOverridableMethods(Handler):
    name = "jdtls_list_overridable_methods"
    method = "java/listOverridableMethods"
    cancel_on_change = True
    send_document_uri = False
    context = None

    def process_request(self, arguments) -> dict:
        self.context = arguments
        return arguments

    def process_response(self, response) -> None:
        if response is not None and len(response) > 0:
            resp = {
                "response": response,
                "context": self.context
            }
            eval_in_emacs("lsp-bridge-jdtls-add-overridable-methods", json.dumps(resp))
            self.context = None
        else:
            message_emacs("No Overridable methods found.")
