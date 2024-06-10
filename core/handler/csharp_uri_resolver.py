import hashlib
import os
import tempfile
from urllib.parse import urlparse, unquote

from core.handler import Handler
from core.utils import *


class CSharpUriResolver(Handler):
    name = "csharp_uri_resolver"
    method = "csharp/metadata"
    cancel_on_change = True
    send_document_uri = False

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.external_file_link = ""
        self.start_pos = None

    def process_request(self, uri, start_pos, define_jump_handler) -> dict:
        self.start_pos = start_pos
        self.external_file_link = uri
        self.define_jump_handler = define_jump_handler
        return dict(textDocument={"uri": uri})

    def process_response(self, response):
        if response is not None:
            external_file_dir = ''
            decompile_dir_name = 'csharp-uri-resolver'

            md5 = hashlib.md5()
            md5.update(self.file_action.get_lsp_server_project_path())
            project_hash = md5.hexdigest()
            data_dir = pathlib.Path(os.path.join(tempfile.gettempdir(), "lsp-bridge-csharp", project_hash))

            external_file_dir = data_dir / decompile_dir_name

            url = urlparse(self.external_file_link)
            path = unquote(url.path)[1:]
            external_file = external_file_dir / path

            external_file.parent.mkdir(exist_ok=True, parents=True)

            if not external_file.exists():
                with open(external_file, 'w') as f:
                    f.write(response["source"])

            external_file = external_file.as_posix()

            self.file_action.create_external_file_action(external_file, self.external_file_link)
            eval_in_emacs(self.define_jump_handler, external_file, get_lsp_file_host(), self.start_pos)
