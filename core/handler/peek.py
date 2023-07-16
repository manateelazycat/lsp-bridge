from core.handler import Handler
from core.utils import *
from typing import Union
import linecache


class PeekFindDefine(Handler):
    name = "peek_find_definition"
    method = "textDocument/definition"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        self.pos = position
        return dict(position=position)

    def process_response(self, response: Union[dict, list]) -> None:
        if not response:
            eval_in_emacs("lsp-bridge-find-def-fallback", self.pos)
            return

        file_info = response[0] if isinstance(response, list) else response
        # volar return only LocationLink (using targetUri)
        file_uri = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
        range1 = file_info["range"] if "range" in file_info else file_info["targetRange"]
        start_pos = range1["start"]

        if file_uri.startswith("jdt://"):
            # for java
            self.file_action.send_server_request(self.file_action.single_server, "jdt_uri_resolver", file_uri, start_pos)
        elif file_uri.startswith("csharp://"):
            # for csharp
            raise NotImplementedError()
        elif file_uri.startswith("jar://"):
            # for clojure
            raise NotImplementedError()
        elif file_uri.startswith("deno:"):
            # for deno
            # Deno will return targetUri like "deno:asset/lib.deno.ns.d.ts",
            # so we need send server deno/virtualTextDocument to request virtual text document from Deno.
            self.file_action.send_server_request(self.file_action.single_server, "deno_uri_resolver", file_uri, start_pos)
        else:
            # for normal file uri
            filepath = uri_to_path(file_uri)
            self.file_action.create_external_file_action(filepath)
            eval_in_emacs("lsp-bridge-peek-define--return", filepath, start_pos)


class PeekFindReferences(Handler):
    name = "peek_find_references"
    method = "textDocument/references"

    def process_request(self, position, define_position) -> dict:
        self.pos = position
        self.define_pos = define_position

        return dict(
            position=position,
            context=dict(includeDeclaration=False)
        )

    def process_response(self, response) -> None:
        if response is None:
            eval_in_emacs("lsp-bridge-find-ref-fallback", self.pos)
        else:
            response = remove_duplicate_references(response)

            references_dict = {}
            for uri_info in response:
                path = uri_to_path(uri_info["uri"])
                if path in references_dict:
                    references_dict[path].append(uri_info["range"])
                else:
                    references_dict[path] = [uri_info["range"]]

            references_counter = 0
            references_content = ""
            for i, (path, ranges) in enumerate(references_dict.items()):
                for rg in ranges:
                    line = rg["start"]["line"]
                    start_column = rg["start"]["character"]

                    # Don't return reference postion if it same as d
                    if line == self.define_pos["line"] and start_column == self.define_pos["character"]:
                        continue

                    references_content += "{}\n{}\n{}\n".format(
                        line,
                        start_column,
                        "".join([path])
                        )
                    references_counter += 1

            linecache.clearcache()  # clear line cache

            eval_in_emacs("lsp-bridge-peek-references--return", references_content, references_counter)
