from core.handler import Handler
from core.utils import *


class InlayHint(Handler):
    name = "inlay_hint"
    method = "textDocument/inlayHint"
    cancel_on_change = True
    provider = "inlay_hint_provider"

    def process_request(self, range_start, range_end) -> dict:
        self.range_start = range_start
        self.range_end = range_end

        range = {
            "start": range_start,
            "end": range_end
        }
        return dict(range=range)

    def process_response(self, response: dict) -> None:
        if response is not None:
            self.file_action.push_inlay_hints(response, self.range_start, self.range_end)
