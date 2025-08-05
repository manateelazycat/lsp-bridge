from core.handler import Handler
from core.utils import *


class IMenu(Handler):
    name = "imenu"
    method = "textDocument/documentSymbol"
    send_document_uri = True

    def process_request(self) -> dict:
        return dict()

    def process_response(self, response: dict) -> None:
        if response is not None:
            try:
                grouped_data = {}

                for item in response:
                    kind = item['kind']
                    if "range" not in item:
                        item["range"] = item['location']["range"]

                    if kind not in grouped_data:
                        grouped_data[kind] = []
                    grouped_data[kind].append(item)

                new_response = []
                for kind, items in grouped_data.items():
                    new_response.append({'name': SYMBOL_MAP[kind], 'children': items})

                eval_in_emacs("lsp-bridge--imenu-show", self.file_action.filepath, get_lsp_file_host(), new_response)
            except:
                import traceback
                logger.error(traceback.format_exc())
