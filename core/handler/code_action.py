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
        
        match_diagnostic = []
        for diagnostic in diagnostics:
            if (range["start"]["line"] >= diagnostic["range"]["start"]["line"] and 
                range["start"]["character"] >= diagnostic["range"]["start"]["character"] and 
                range["end"]["line"] <= diagnostic["range"]["end"]["line"] and 
                range["end"]["character"] <= diagnostic["range"]["end"]["character"]):
                match_diagnostic.append(diagnostic)
                break
        
        context = {
            "diagnostics": match_diagnostic,
            "only": kinds if kind == "" else [kind]
        }
        
        return dict(range=range, context=context)

    def process_response(self, response) -> None:
        if response != None:
            for solution in response:
                if solution.get("kind", "") == "quickfix":
                    change_infos = []
                    for (uri, info) in solution["edit"]["changes"].items():
                        change_infos.append([uri_to_path(uri), info])
                    
                    eval_in_emacs("lsp-bridge-code-action-quickfix", solution.get("title", ""), change_infos)
                else:
                    print("***** ", solution)
        
