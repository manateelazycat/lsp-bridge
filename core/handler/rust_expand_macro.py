from core.handler import Handler
from core.utils import *
from typing import Union

class RustExpandMacro(Handler):
    name = "rust_expand_macro"
    method = "rust-analyzer/expandMacro"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        self.pos = position
        return dict(position=position)

    def process_response(self, response: Union[dict, list]) -> None:
        if response is None:
            print("Error: Received None response from rust-analyzer")
            return

        if "name" in response and "expansion" in response:
            eval_in_emacs("lsp-bridge-rust-expand-macro--update", response["name"], response["expansion"])
        else:
            print(f"Error: Response missing 'name' or 'expansion' keys. Response: {response}")
