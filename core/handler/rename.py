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
        
        rename_infos = response["documentChanges"] if "documentChanges" in response else response["changes"]

        if type(rename_infos) == dict:
            # JSON struct is 'changes'
            for rename_info in rename_infos.items():
                eval_in_emacs("lsp-bridge-rename-file", uri_to_path(rename_info[0]), rename_info[1])
        else:
            # JSON struct is 'documentChanges'
            for rename_info in rename_infos:
                eval_in_emacs("lsp-bridge-rename-file", uri_to_path(rename_info["textDocument"]["uri"]), rename_info["edits"])

        message_emacs("Rename done.")
