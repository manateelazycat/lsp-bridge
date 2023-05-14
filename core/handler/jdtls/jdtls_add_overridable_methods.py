from core.handler import Handler
from core.utils import *


class JdtlsAddOverridableMethods(Handler):
    name = "jdtls_add_overridable_methods"
    method = "java/addOverridableMethods"
    cancel_on_change = True
    send_document_uri = False

    def process_request(self, params) -> dict:
        return parse_json_content(params)

    def process_response(self, response) -> None:
        if response is not None and len(response) > 0:
            eval_in_emacs("lsp-bridge-workspace-apply-edit", response)
