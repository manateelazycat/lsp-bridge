from core.handler import Handler
from core.utils import *


class CSharpUriResolver(Handler):
    name = "csharp_uri_resolver"
    method = "csharp/metadata"
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

    def process_response(self, response, project_path):
        if response is not None:
            # Find the path to the C# solution file
            solution_path = find_csharp_solution_file(project_path)

            if solution_path is not None:
                define_file_path = os.path.join(
                    solution_path,
                    ".cache",
                    "csharp-ls",
                    "metadata",
                    "projects",
                    response["projectName"],
                    "assemblies",
                    response["assemblyName"],
                    "{}.cs".format(response["symbolName"])
                )

                # We need build cache file first.
                touch(define_file_path)

                # Write source code to cache file.
                with open(define_file_path, "w") as f:
                    f.write(response["source"])

                eval_in_emacs(self.define_jump_handler, define_file_path, get_lsp_file_host(), self.start_pos)
