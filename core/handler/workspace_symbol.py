from core.handler import Handler
from core.utils import *


class WorkspaceSymbol(Handler):
    name = "workspace_symbol"
    method = "workspace/symbol"
    provider = "workspace_symbol_provider"
    provider_message = "Current server not support workspace symbol."

    def process_request(self, query) -> dict:
        query = ''.join(query.split())
        return dict(query=query)

    def process_response(self, response: dict) -> None:
        if response is not None:
            remote_connection_info = get_remote_connection_info()
            for item in response:
                item["location"]["uri"] = local_path_to_tramp_path(item["location"]["uri"],
                                                                   remote_connection_info)

            eval_in_emacs("lsp-bridge-workspace--list-symbols", response)
