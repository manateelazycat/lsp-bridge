from typing import Optional
from core.handler import Handler


class Diagnostic(Handler):
    """
    Handler for textDocument/diagnostic request
    """

    name = "diagnostic"
    method = "textDocument/diagnostic"
    provider = "diagnostic_provider"
    provider_message = "LSP server doesn't support pull-based diagnostics"

    def process_request(self, identifier: Optional[str] = None, previous_result_id: Optional[str] = None) -> dict:
        params = {}
        if identifier is not None:
            params["identifier"] = identifier
        if previous_result_id is not None:
            params["previousResultId"] = previous_result_id
        return params

    def process_response(self, response: dict) -> None:
        if not response or "items" not in response:
            return

        diagnostics = response.get("items")

        self.file_action.record_diagnostics(diagnostics, self.server_info["name"])
