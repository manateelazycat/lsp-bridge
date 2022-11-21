from core.handler import Handler
from core.utils import *

class PrepareCallHierarchy(Handler):
    name = "_prepare_call_hierarchy"
    method = "textDocument/prepareCallHierarchy"
    send_document_uri = True

    method_call_hierarchy: str
    response_prefix: str

    def process_request(self, position) -> dict:
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        if response is None:
            message_emacs("No call hierarchies found")
        else:
            self.file_action.call(self.method_call_hierarchy, response[0], self.response_prefix)
            
class CallHierarchy(Handler):
    name = "_call_hierarchy"
    send_document_uri = False

    response_prefix: str
    call_contents = []

    def process_request(self, item, prefix) -> dict:
        self.response_prefix = prefix
        # add current object to call_contents
        self.add_call_content(item, [])
        return dict(item=item)

    def add_call_content(self, item, from_ranges):
        self.call_contents.append({'name': item['name'],
                                   'path': uri_to_path(item['uri']),
                                   'icon': SYMBOL_MAP[item['kind']].lower(),
                                   'range': item['range'],
                                   'from_ranges': from_ranges})

    def process_response(self, response: dict) -> None:
        for call_info in response:
            call = call_info[self.response_prefix]
            from_ranges = call_info['fromRanges']
            self.add_call_content(call, from_ranges)

        base_dir = os.path.dirname(self.file_action.filepath)
        for call in self.call_contents:
            rel_path = os.path.relpath(call['path'], base_dir)    # type: ignore
            if '../../../../' not in rel_path:    # type: ignore
                call['rel_path'] = rel_path
            else:
                call['rel_path'] = call['path']

        eval_in_emacs("lsp-bridge-call-hierarchy--popup", self.call_contents)
        
        self.call_contents = []

class PrepareCallHierarchyIncomingCalls(PrepareCallHierarchy, Handler):
    name = "prepare_call_hierarchy_incoming"
    method_call_hierarchy = "call_hierarchy_incoming"
    response_prefix = "from"

class PrepareCallHierarchyOutgoingCalls(PrepareCallHierarchy, Handler):
    name = "prepare_call_hierarchy_outgoing"
    method_call_hierarchy = "call_hierarchy_outgoing"
    response_prefix = "to"

class CallHierarchyIncomingCalls(CallHierarchy, Handler):
    name = "call_hierarchy_incoming"
    method = "callHierarchy/incomingCalls"

class CallHierarchyOutgoingCalls(CallHierarchy, Handler):
    name = "call_hierarchy_outgoing"
    method = "callHierarchy/outgoingCalls"
    
