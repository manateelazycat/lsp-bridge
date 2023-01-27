from core.handler import Handler
from core.utils import *


class WorkspaceSymbol(Handler):
    name = "workspace_symbol"
    method = "workspace/symbol"
    provider = "workspace_symbol_provider"

    def process_request(self, query) -> dict:
        query = ''.join(query.split())
        return dict(query=query)

    def process_response(self, response: dict) -> None:
        if response is not None:
            eval_in_emacs("lsp-bridge-workspace--list-symbols", response)
