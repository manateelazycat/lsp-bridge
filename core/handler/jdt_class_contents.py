from core.handler import Handler
from core.utils import *

import os
import re

class JDTClassContents(Handler):
    name = "jdt_class_contents"
    method = "java/classFileContents"
    cancel_on_change = True
    lsp_buffer_uri = self.fa.filepath
    start_pos = None

    def process_request(self) -> dict:
        return dict()

    def send_request(self, file_uri, start_pos):
        self.latest_request_id = request_id = generate_request_id()
        self.last_change = self.fa.last_change
        self.start_pos = start_pos
        self.lsp_buffer_uri = file_uri

        self.fa.lsp_server.record_request_id(
            request_id=request_id,
            method=self.method,
            filepath=self.fa.filepath,
            type=self.name,
        )

        params = self.process_request()
        params["uri"] = file_uri

        self.fa.lsp_server.send_to_request(
            name=self.method,
            params=params,
            request_id=request_id,
        )


    def process_response(self, response: str) -> None:
        if not response:
            message_emacs("No definition found.")
            return

        if type(response) == str:
            if not os.path.exists(self.fa.java_workspace_cache_dir):
                os.mkdir(self.fa.java_workspace_cache_dir)
            doc_name = re.match(r"jdt://contents/(.*?)/(.*)\.class\?", self.lsp_buffer_uri).groups()[1].replace('/', '.') + ".java"
            doc_file = os.path.join(self.fa.java_workspace_cache_dir, doc_name)
            if not os.path.exists(doc_file):
                with open(doc_file, 'w') as f:
                    f.write(response)
            self.fa.lsp_server.send_did_open_notification(doc_file, self.lsp_buffer_uri)
            eval_in_emacs("lsp-bridge--jump-to-def", doc_file, self.start_pos, True)
