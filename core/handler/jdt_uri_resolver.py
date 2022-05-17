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
        self.start_pos = None

    def process_request(self, uri, start_pos) -> dict:
        self.start_pos = start_pos
        return dict(uri=uri)

    def process_response(self, response: str):
        if not response:
            message_emacs("No definition found.")
            return

        if type(response) == str:
            doc_file = self.file_action.filepath
            doc_dir = os.path.dirname(doc_file)
            os.makedirs(doc_dir, exist_ok=True)
            if not os.path.exists(doc_file):
                with open(doc_file, 'w') as f:
                    f.write(response)
            self.file_action.lsp_server.send_did_open_notification(doc_file)
            eval_in_emacs("lsp-bridge--jump-to-def", doc_file, self.start_pos)
