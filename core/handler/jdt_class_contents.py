from core.handler import Handler
from core.utils import *

import os
import re

class JDTClassContents(Handler):
    name = "jdt_class_contents"
    method = "java/classFileContents"
    cancel_on_change = True

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.lsp_buffer_uri = None
        self.start_pos = None

    def process_request(self, uri, start_pos) -> dict:
        self.start_pos = start_pos
        self.lsp_buffer_uri = uri
        return dict(uri=uri)

    def process_response(self, response: str) -> None:
        if not response:
            message_emacs("No definition found.")
            return

        if type(response) == str:
            if not os.path.exists(self.fa.workspace_cache_dir):
                os.mkdir(self.fa.workspace_cache_dir)
            doc_name = re.match(r"jdt://contents/(.*?)/(.*)\.class\?", self.lsp_buffer_uri).groups()[1].replace('/', '.') + ".java"
            doc_file = os.path.join(self.fa.workspace_cache_dir, doc_name)
            if not os.path.exists(doc_file):
                with open(doc_file, 'w') as f:
                    f.write(response)
            self.fa.lsp_server.send_did_open_notification(doc_file, self.lsp_buffer_uri)
            eval_in_emacs("lsp-bridge--jump-to-def", doc_file, self.start_pos, True)
