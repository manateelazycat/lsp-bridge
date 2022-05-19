from core.handler import Handler
from core.utils import *
import core.fileaction
import os
import re
import hashlib
import tempfile

class JDTUriResolver(Handler):
    name = "jdt_uri_resolver"
    method = "java/classFileContents"
    cancel_on_change = True

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.external_file_link = None
        self.start_pos = None

    def process_request(self, uri, start_pos) -> dict:
        self.start_pos = start_pos
        self.external_file_link = uri
        return dict(uri=uri)

    def process_response(self, response: str):
        if not response:
            message_emacs("No definition found.")
            return

        if type(response) == str:
            # Save the analysis content to the file.

            external_file_dir = ''
            decompile_dir_name = 'jdt-uri-resolver'

            try:
                # Value for -data: An absolute path to your data directory. eclipse.jdt.ls stores workspace specific information in it. This should be unique per workspace/project.
                index = self.file_action.lang_server_info['command'].index('-data')
                data_dir = self.file_action.lang_server_info['command'][index + 1]
                external_file_dir = os.path.join(data_dir, decompile_dir_name)
            except ValueError as e:
                md5 = hashlib.md5()
                md5.update(self.file_action.lsp_server.project_path.encode('utf-8'))
                project_hash = md5.hexdigest()
                external_file_dir = os.path.join(tempfile.gettempdir(),
                                                 "lsp-bridge-jdtls",
                                                 project_hash,
                                                 decompile_dir_name)

            external_file = os.path.join(
                external_file_dir,
                re.match(r"jdt://contents/(.*?)/(.*)\.class\?", self.external_file_link).groups()[1].replace('/', '.') + ".java")

            os.makedirs(external_file_dir, exist_ok=True)

            # Always override decompile content to file, avoid cache conflict.
            with open(external_file, 'w') as f:
                f.write(response)

            # Jump to define in external file.
            self.file_action.lsp_server.message_queue.put({
                "name": "jump_to_external_file_link",
                "content": {
                    "filepath": external_file,
                    "external_file_link": self.external_file_link,
                    "file_action": self.file_action,
                    "start_pos": self.start_pos
                }
            })

    def fill_document_uri(self, params: dict) -> None:
        # JDT don't need fill textDocument uri.
        pass
