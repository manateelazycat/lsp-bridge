from core.handler import Handler
from core.utils import *
import linecache

REFERENCE_PATH = '\033[95m'
REFERENCE_TEXT = '\033[94m'
REFERENCE_ENDC = '\033[0m'


class FindReferences(Handler):
    name = "find_references"
    method = "textDocument/references"

    def process_request(self, position) -> dict:
        return dict(
            position=position,
            context=dict(includeDeclaration=False)
        )

    def process_response(self, response: dict) -> None:
        if response is None:
            message_emacs("No references found")
        else:
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
                references_content += "\n" + REFERENCE_PATH + path + REFERENCE_ENDC + "\n"

                for rg in ranges:
                    with open(path, encoding="utf-8", errors="ignore") as f:
                        line = rg["start"]["line"]
                        start_column = rg["start"]["character"]
                        end_column = rg["end"]["character"]
                        line_content = linecache.getline(path, rg["start"]["line"] + 1)

                        references_content += "{}:{}:{}".format(
                            line + 1,
                            start_column,
                            line_content[:start_column] + REFERENCE_TEXT +
                            line_content[start_column:end_column] + REFERENCE_ENDC +
                            line_content[end_column:])
                        references_counter += 1

            linecache.clearcache()  # clear line cache

            eval_in_emacs("lsp-bridge-popup-references", references_content, references_counter)
