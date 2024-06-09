from core.handler import Handler
from core.handler.find_define_base import find_define_response
from core.utils import *
from typing import Union


class FindDefine(Handler):
    name = "find_define"
    method = "textDocument/definition"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        self.pos = position
        return dict(position=position)

    def process_response(self, response: Union[dict, list]) -> None:
        find_define_response(self, response, "lsp-bridge-define--jump")
