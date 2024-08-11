from core.handler import Handler
from core.utils import *

class WorkspaceDiagnostics(Handler):
    name = "workspace_diagnostics"
    method = "workspace/diagnostic"
    cancel_on_change = False
    send_document_uri = False

    def process_request(self) -> dict:
        return dict(previousResultIds=[])

    def process_response(self, response: dict) -> None:
        print("****** ", response)
