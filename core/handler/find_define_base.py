import hashlib
import os
import tempfile
from urllib.parse import urlparse, unquote

from core.utils import *

def find_define_response(obj, response, define_jump_handler) -> None:
    if not response:
        eval_in_emacs("lsp-bridge-find-def-fallback", obj.pos)
        return

    file_info = response[0] if isinstance(response, list) else response
    # volar return only LocationLink (using targetUri)
    file_uri = file_info["uri"] if "uri" in file_info else file_info["targetUri"]
    define_range = file_info["range"] if "range" in file_info else file_info["targetRange"]
    start_pos = define_range["start"]

    if file_uri.startswith("jdt://"):
        # for java
        message_emacs("Resolve path {} ...".format(file_uri))
        obj.file_action.send_server_request(obj.file_action.single_server, "jdt_uri_resolver", file_uri, start_pos, define_jump_handler)
    elif file_uri.startswith("csharp:/metadata/"):
        # for csharp
        message_emacs("Resolve path {} ...".format(file_uri))
        obj.file_action.send_server_request(obj.file_action.single_server, "csharp_uri_resolver", file_uri, start_pos, define_jump_handler)
    elif file_uri.startswith("jar://"):
        # for clojure
        raise NotImplementedError()
    elif file_uri.startswith("deno:"):
        # for deno
        # Deno will return targetUri like "deno:asset/lib.deno.ns.d.ts",
        # so we need send server deno/virtualTextDocument to request virtual text document from Deno.
        message_emacs("Resolve path {} ...".format(file_uri))
        obj.file_action.send_server_request(obj.file_action.single_server, "deno_uri_resolver", file_uri, start_pos, define_jump_handler)
    else:
        # for normal file uri
        filepath = uri_to_path(file_uri)
        obj.file_action.create_external_file_action(filepath)
        eval_in_emacs(define_jump_handler, filepath, get_lsp_file_host(), start_pos)

def create_decompile_external_file(uri_resolver, language_dir, decompile_dir, source_code):
    '''Some LSP server, such as Java or C#, LSP server need decompile source code to *external* file before jump to definition in *original* file.
    Base on lsp-bridge default policy, lsp-bridge will create NEW LSP server for *external* file if *external* file own by different directory.
    If *external* file use *new* LSP server, `lsp-bridge-find-def` won't response in *external* file,
    because *new* LSP server don't know project information about *original* project.

    So we call `create_external_file_action` on *external* file, pass *original* file's LSP server to FileAction structural function,
    to make sure *external* file share SAME LSP server with *original* file.

    Last, we can do `lsp-bridge-find-def` in *external* file.
    '''
    # Init.
    external_file_dir = ''

    # Build temp directory base on project hash.
    md5 = hashlib.md5()
    md5.update(uri_resolver.file_action.get_lsp_server_project_path())
    project_hash = md5.hexdigest()
    data_dir = pathlib.Path(os.path.join(tempfile.gettempdir(), language_dir, project_hash))
    external_file_dir = data_dir / decompile_dir

    # Build temp file path.
    url = urlparse(uri_resolver.external_file_link)
    path = unquote(url.path)[1:] # remove first / from path to make `external_file_dir` can join `path` at below
    external_file = external_file_dir / path

    # Create temp file if is not exists.
    external_file.parent.mkdir(exist_ok=True, parents=True)

    # Write source code to temp file.
    if not external_file.exists():
        with open(external_file, 'w') as f:
            f.write(source_code)

    external_file = external_file.as_posix()

    # Create external file action, make sure external file share one LSP server with current file.
    uri_resolver.file_action.create_external_file_action(external_file, uri_resolver.external_file_link)

    return external_file
