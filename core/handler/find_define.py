import os
import re
from urllib.parse import urlparse

import core.fileaction
from core.handler import Handler
from core.utils import *
from typing import Union


class FindDefine(Handler):
    name = "find_define"
    method = "textDocument/definition"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: Union[dict, list[dict]]) -> None:
        if not response:
            message_emacs("No definition found.")
            return

        file_info = response[0] if isinstance(response, list) else response
        # volar return only LocationLink (using targetUri)
        file_uri = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
        range1 = file_info["range"] if "range" in file_info else file_info["targetRange"]
        start_pos = range1["start"]

        if file_uri.startswith("jdt://"):
            # for java
            doc_name = re.match(r"jdt://contents/(.*?)/(.*)\.class\?", file_uri).groups()[1].replace('/', '.') + ".java"
            doc_file = os.path.join(self.file_action.lsp_server.library_directories[0], doc_name)
            file_action = core.fileaction.FileAction(doc_file, self.file_action.project_path, self.file_action.lang_server_info["name"])
            file_action.lsp_location_link = file_uri
            file_action.lsp_server = self.file_action.lsp_server
            # FIXME: 异步问题？收到此消息与_open_file被调用的顺序
            self.file_action.lsp_server.message_queue.put({
                "name": "make_lsp_location_link_file_action",
                "content": {
                    "filepath": doc_file,
                    "file_action": file_action
                }
            })
            file_action.handlers["jdt_uri_resolver"].send_request(file_uri, start_pos)
        elif file_uri.startswith("csharp://"):
            # for csharp
            raise NotImplementedError()
        elif file_uri.startswith("jar://"):
            # for clojure
            raise NotImplementedError()
        else:
            # for normal file uri
            filepath = uri_to_path(file_uri)
            eval_in_emacs("lsp-bridge--jump-to-def", filepath, start_pos)
