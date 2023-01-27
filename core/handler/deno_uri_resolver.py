import os

from core.handler import Handler
from core.utils import *


class DenoUriResolver(Handler):
    name = "deno_uri_resolver"
    method = "deno/virtualTextDocument"
    cancel_on_change = True
    send_document_uri = False

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.external_file_link = ""
        self.start_pos = None

    def process_request(self, uri, start_pos) -> dict:
        self.start_pos = start_pos
        self.external_file_link = uri
        return dict(textDocument={"uri": uri})

    def process_response(self, response):
        if response is not None:
            import tempfile
            deno_virtual_text_document_path = os.path.join(tempfile.gettempdir(), self.external_file_link.split("/")[-1])
            with open(deno_virtual_text_document_path, "w") as f:
                f.write(response)    # type: ignore
                
            eval_in_emacs("lsp-bridge-define--jump", deno_virtual_text_document_path, self.start_pos)
