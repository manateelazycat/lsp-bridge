from core.handler import Handler
from core.utils import *


class CodeAction(Handler):
    name = "code_action"
    method = "textDocument/codeAction"
    cancel_on_change = True
    provider = "code_action_provider"
    provider_message = "Current server not support code action."

    def process_request(self, lsp_server_name, diagnostics, range_start, range_end, action_kind) -> dict:
        self.action_kind = action_kind
        self.lsp_server_name = lsp_server_name
        
        range = {
            "start": range_start,
            "end": range_end
        }
        
        if isinstance(action_kind, str):
            context = {
                "diagnostics": diagnostics,
                "only": [action_kind]
            }
        else:
            context = {
                "diagnostics": diagnostics
            }
            
        return dict(range=range, context=context)

    def process_response(self, response) -> None:
        remote_connection_info = get_remote_connection_info()
        if remote_connection_info != "" :
            for item in response:
                changes = item["edit"]["changes"]
                new_changes = {}
                for file in changes.keys():
                    tramp_file = local_path_to_tramp_path(file, remote_connection_info)
                    new_changes[tramp_file] = changes[file]
                item["edit"]["changes"] = new_changes

        self.file_action.push_code_actions(response, self.lsp_server_name, self.action_kind)
