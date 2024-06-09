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
