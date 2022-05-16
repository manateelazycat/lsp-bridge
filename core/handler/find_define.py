from core.handler import Handler
from core.utils import *
from core.handler.jdt_class_contents import JDTClassContents


class FindDefine(Handler):
    name = "find_define"
    method = "textDocument/definition"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        if not response:
            message_emacs("No definition found.")
            return

        file_info = response[0]
        range1 = file_info["range"] if "range" in file_info else file_info["targetRange"]
        # volar return only LocationLink (using targetUri)
        file_uri: str = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
        start_pos = range1["start"]
        if file_uri.startswith("jdt://"):
            # for java
            self.fa.handlers[JDTClassContents.name].send_request(file_uri, start_pos)
        elif file_uri.startswith("csharp://"):
            # for csharp
            raise NotImplementedError()
        elif file_uri.startswith("jar://"):
            # for clojure
            raise NotImplementedError()
        else:
            # for normal file uri
            filepath = uri_to_path(file_uri)
            eval_in_emacs("lsp-bridge--jump-to-def", filepath, start_pos, False)
