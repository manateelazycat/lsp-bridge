from core.handler import Handler
from core.utils import *


class DocumentHighlight(Handler):
    name = "document_highlight"
    method = "textDocument/documentHighlight"
    cancel_on_change = True
    provider = "document_highlight_provider"
    provider_message = "LSP server does not support documentHighlight"

    def process_request(self, position) -> dict:
        # textDocument will be injected by FileAction.send_server_request
        return {"position": position}

    def process_response(self, response: dict) -> None:
        if response is not None:
            eval_in_emacs("lsp-bridge-document-highlight-render", response)
