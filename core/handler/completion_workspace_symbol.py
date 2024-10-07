from core.handler import Handler
from core.utils import *


class CompletionWorkspaceSymbol(Handler):
    name = "completion_workspace_symbol"
    method = "workspace/symbol"
    provider = "workspace_symbol_provider"

    def process_request(self, lsp_server, query) -> dict:
        self.lsp_server = lsp_server
        self.lsp_server_name = self.lsp_server.server_info["name"]
        query = ''.join(query.split())
        return dict(query=query)

    def process_response(self, response: dict) -> None:
        if response is not None:
            completion_symbols = []

            for item in response:
                symbol_name = item["name"]
                symbol_kind = KIND_MAP[item.get("kind", 0)].lower()
                symbol_display_name = "{} [{}]".format(symbol_name, symbol_kind)
                symbol = {
                    "key": symbol_name,
                    "icon": "workspace-symbol",
                    "label": symbol_name,
                    "displayLabel": symbol_display_name,
                    "server": self.lsp_server_name,
                    "backend": "lsp-workspace-symbol"
                }

                completion_symbols.append(symbol)

            if len(completion_symbols) > 0:
                eval_in_emacs("lsp-bridge-completion-workspace-symbol--record-items",
                              self.file_action.filepath,
                              get_lsp_file_host(),
                              completion_symbols,
                              self.lsp_server_name,
                              self.file_action.get_lsp_server_names())
