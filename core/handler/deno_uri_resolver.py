
from core.handler import Handler
from core.handler.find_define_base import create_decompile_external_file
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

    def process_request(self, uri, start_pos, define_jump_handler) -> dict:
        self.start_pos = start_pos
        self.external_file_link = uri
        self.define_jump_handler = define_jump_handler
        return dict(textDocument={"uri": uri})

    def process_response(self, response):
        if response is not None:
            external_file = create_decompile_external_file(
                self,
                "lsp-bridge-deno",
                "deno-uri-resolver",
                response)

            eval_in_emacs(self.define_jump_handler, external_file, get_lsp_file_host(), self.start_pos)
