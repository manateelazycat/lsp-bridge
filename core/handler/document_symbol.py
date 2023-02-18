from core.handler import Handler
from core.utils import *


class DocumentSymbol(Handler):
    name = "document_symbol"
    method = "textDocument/documentSymbol"
    send_document_uri = True
    position = None

    def process_request(self, position) -> dict:
        self.position = position
        return dict()

    def process_response(self, response: dict) -> None:
        symbols = []
        current_defun = ''
        for symbol in response:
            range = symbol['location']["range"]
            if range['start']['line'] < self.position['line'] < range['end']['line'] or\
               (range['start']['line'] == self.position['line'] and range['start']['character'] <= self.position['character']) or\
               (range['end']['line'] == self.position['line'] and range['end']['character'] >= self.position['character']):
                if len(current_defun):
                    current_defun += '.'
                current_defun += symbol['name']
                symbols.append(symbol)
        eval_in_emacs("lsp-bridge-symbols--record-current-defun", current_defun)
