from core.handler import Handler
from core.utils import *

import core.fileaction

import os
import re

class JDTUriResolver(Handler):
    name = "jdt_uri_resolver"
    method = "java/classFileContents"
    cancel_on_change = True

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.lsp_location_link = None
        self.start_pos = None

    def process_request(self, uri, start_pos) -> dict:
        self.start_pos = start_pos
        self.lsp_location_link = uri
        return dict(uri=uri)

    def process_response(self, response: str):
        if not response:
            message_emacs("No definition found.")
            return

        if type(response) == str:
            doc_name = re.match(r"jdt://contents/(.*?)/(.*)\.class\?", self.lsp_location_link).groups()[1].replace('/', '.') + ".java"
            doc_file = os.path.join(self.file_action.lsp_server.library_directories[0], doc_name)
            if not os.path.exists(doc_file):
                with open(doc_file, 'w') as f:
                    f.write(response)
            # may need optimization
            file_action = core.fileaction.FileAction(doc_file, self.file_action.project_path, self.file_action.lang_server_info["name"])
            file_action.lsp_location_link = self.lsp_location_link
            file_action.lsp_server = self.file_action.lsp_server
            self.file_action.lsp_server.message_queue.put({
                "name": "make_lsp_location_link_file_action",
                "content": {
                    "filepath": doc_file,
                    "file_action": file_action
                }
            })
            self.file_action.lsp_server.send_did_open_notification(doc_file, self.lsp_location_link)
            eval_in_emacs("lsp-bridge--jump-to-def", doc_file, self.start_pos)
