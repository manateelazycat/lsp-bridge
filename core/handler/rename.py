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

        remote_connection_info = get_remote_connection_info()
        logger.info(response)
        if remote_connection_info != "" :
            changes = response["changes"]
            new_changes = {}
            for file in changes.keys():
                tramp_file = local_path_to_tramp_path(file, remote_connection_info)
                new_changes[tramp_file] = changes[file]
            response["changes"] = new_changes

        eval_in_emacs("lsp-bridge-workspace-apply-edit", response)
        
        message_emacs("Rename done.")
