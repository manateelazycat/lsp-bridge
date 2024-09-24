from core.handler import Handler
from core.utils import *

class RustReloadWorkspace(Handler):
    name = "rust_reload_workspace"
    method = "rust-analyzer/reloadWorkspace"
    cancel_on_change = False
    send_document_uri = False

    def process_request(self) -> dict:
        return None

    def process_response(self, response) -> None:
        pass
