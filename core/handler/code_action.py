from core.handler import Handler
from core.utils import *
from typing import Union


class CodeAction(Handler):
    name = "code_action"
    method = "textDocument/codeAction"
    cancel_on_change = True

    def process_request(self, range_start, range_end, diagnostics, kinds, kind) -> dict:
        range = {
            "start": range_start,
            "end": range_end
        }
        
        context = {
            "diagnostics": diagnostics,
            "only": kinds if kind == "" else [kind]
        }
        
        return dict(range=range, context=context)

    def process_response(self, response) -> None:
        print("**** ", response)
