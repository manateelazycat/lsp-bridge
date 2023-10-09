from core.handler import Handler
from core.utils import *

class CompletionItem(Handler):
    name = "completion_item_resolve"
    method = "completionItem/resolve"
    cancel_on_change = False
    send_document_uri = False

    def process_request(self, item_key, server_name, item) -> dict:
        self.item_key = item_key
        self.server_name = server_name
        return item

    def process_response(self, response: dict) -> None:
        response_doc = ""
        additional_text_edits = []
        
        if response is not None:
            if "documentation" in response:
                response_doc = response["documentation"]
                if isinstance(response_doc, dict):
                    if "value" in response_doc:
                        response_doc = response_doc["value"]
                    else:
                        response_doc = ""

            if len(response_doc) == 0 and "detail" in response:
                response_doc = response["detail"]

            if "additionalTextEdits" in response:
                additional_text_edits = response["additionalTextEdits"]

        self.file_action.completion_item_update(self.item_key, self.server_name, response_doc.strip(), additional_text_edits)
