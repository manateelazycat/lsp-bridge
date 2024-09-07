from core.handler import Handler
from core.utils import *


class InlayHint(Handler):
    name = "inlay_hint"
    method = "textDocument/inlayHint"
    cancel_on_change = True
    provider = "inlay_hint_provider"
    provider_message = "Current server not support inlay hint."

    def process_request(self, range_start, range_end) -> dict:
        range = {
            "start": range_start,
            "end": range_end
        }
        return dict(range=range)

    def process_response(self, response: dict) -> None:
        if response is not None:
            inlay_hints = {}

            for hint in response:
                key = "{}-{}".format(hint["position"]["line"], hint["position"]["character"])
                inlay_hints[key] = hint

            inlay_hints = {k: inlay_hints[k] for k in sorted(inlay_hints, key=lambda x: [int(i) for i in x.split('-')])}

            inlay_hints = list(reversed(inlay_hints.values()))

            eval_in_emacs("lsp-bridge-inlay-hint--render",
                          self.file_action.filepath,
                          get_lsp_file_host(),
                          inlay_hints)
