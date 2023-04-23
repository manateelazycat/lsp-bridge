from core.handler import Handler
from core.utils import *


class FindImplementation(Handler):
    name = "find_implementation"
    method = "textDocument/implementation"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        if not response:
            message_emacs("No implementation found.")
            return

        infos = []
        for file_info in response:
            # volar return only LocationLink (using targetUri)
            fileuri = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
            filepath = uri_to_path(fileuri)

            range = file_info["range"] if "range" in file_info else file_info["targetRange"]
            pos = range["start"]
            line = pos["line"]
            character = pos["character"]

            with open(filepath, 'r') as file:
                content = file.readlines()[pos["line"]].rstrip()

            if content[-1] in "({[":
                content = content[:-1]

            infos.append({
                "filepath": filepath,
                "position": pos,
                "id": f"{filepath}:{content} [{line}:{character}]"
            })

        eval_in_emacs("lsp-bridge-impl--jump", get_lsp_file_host(), infos)
