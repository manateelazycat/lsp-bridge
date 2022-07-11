from core.handler import Handler
from core.utils import *

class Rename(Handler):
    name = "rename"
    method = "textDocument/rename"

    def process_request(self, position, new_name) -> dict:
        return dict(position=position, newName=new_name)

    def process_response(self, response: dict) -> None:
        if response is None:
            logger.info("No rename found.")
            message_emacs("No rename found")
            return
        
        eval_in_emacs("lsp-bridge-workspace-apply-edit", response)
        
        message_emacs("Rename done.")
