from core.handler import Handler
from core.utils import *

SYMBOL_KIND_NAMES = {
    1: "File", 2: "Module", 3: "Namespace", 4: "Package", 5: "Class",
    6: "Method", 7: "Property", 8: "Field", 9: "Constructor",
    10: "Enum", 11: "Interface", 12: "Function", 13: "Variable",
    14: "Constant", 15: "String", 16: "Number", 17: "Boolean",
    18: "Array", 19: "Object", 20: "Key", 21: "Null",
    22: "EnumMember", 23: "Struct", 24: "Event", 25: "Operator",
    26: "TypeParameter"
}

class DocumentSymbol(Handler):
    name = "document_symbol"
    method = "textDocument/documentSymbol"
    send_document_uri = True
    position = None

    def process_request(self, position) -> dict:
        self.position = position
        return dict()

    def process_response(self, response: dict) -> None:
        if response is not None:
            try:
                if not bool(self.position):
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
                        new_response.append({'name': SYMBOL_KIND_NAMES[kind], 'children': items})

                    eval_in_emacs("lsp-bridge--imenu-show",
                                  self.file_action.filepath,
                                  get_lsp_file_host(),
                                  new_response)
                else:
                    symbols = []
                    current_defun = ''
                    for symbol in response:
                        if "range" in symbol:
                            range = symbol["range"]
                        else:
                            range = symbol['location']["range"]
                        if range['start']['line'] < self.position['line'] < range['end']['line'] or\
                           (range['start']['line'] == self.position['line'] and range['start']['character'] <= self.position['character']) or\
                           (range['end']['line'] == self.position['line'] and range['end']['character'] >= self.position['character']):
                            if len(current_defun):
                                current_defun += '.'
                                current_defun += symbol['name']
                                symbols.append(symbol)
                    eval_in_emacs("lsp-bridge-symbols--record-current-defun", current_defun)
            except:
                import traceback
                logger.error(traceback.format_exc())
