from core.handler import Handler
from core.utils import *

class _CallHierarchy(Handler):
    name = '_call_hierarchy'
    method = "textDocument/prepareCallHierarchy"

    method_call_hierarchy: str
    response_prefix: str

    call_contents = []

    def reset_handler(self):
        self.send_document_uri = True
        self.method = "textDocument/prepareCallHierarchy"
        self.call_contents = []

    def process_request(self, position_or_items) -> dict:
        if self.method == 'textDocument/prepareCallHierarchy':
            return dict(position=position_or_items)
        else:
            return dict(item=position_or_items)

    def add_call_content(self, item, from_ranges):
        self.call_contents.append({'name': item['name'],
                                   'path': uri_to_path(item['uri']),
                                   'icon': SYMBOL_MAP[item['kind']].lower(),
                                   'range': item['range'],
                                   'from_ranges': from_ranges})

    def process_response(self, response: dict) -> None:
        if response is None:
            self.reset_handler()
            message_emacs("No call hierarchies found")
        elif self.method == 'textDocument/prepareCallHierarchy':
            self.method = self.method_call_hierarchy

            self.send_document_uri = False

            # TODO allow selecting items
            item = response[0]

            self.file_action.call(self.name, item)

            self.add_call_content(item, [])
        else:
            prefix = self.response_prefix

            for call_info in response:
                call = call_info[prefix]
                from_ranges = call_info['fromRanges']
                self.add_call_content(call, from_ranges)

            base_dir = os.path.dirname(self.file_action.filepath)
            for call in self.call_contents:
                rel_path = os.path.relpath(call['path'], base_dir)
                if '../../../../' not in rel_path:
                    call['rel_path'] = rel_path
                else:
                    call['rel_path'] = call['path']

            eval_in_emacs("lsp-bridge-call-hierarchy--popup", self.call_contents)

            self.reset_handler()


class CallHierarchyIncomingCalls(_CallHierarchy, Handler):
    name = "call_hierarchy_incoming"
    method_call_hierarchy = "callHierarchy/incomingCalls"
    response_prefix = "from"


class CallHierarchyOutgoingCalls(_CallHierarchy, Handler):
    name = "call_hierarchy_outgoing"
    method_call_hierarchy = "callHierarchy/outgoingCalls"
    response_prefix = "to"
