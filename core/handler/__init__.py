import abc
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from core.fileaction import FileAction
from core.utils import *

class Handler(abc.ABC):
    name: str  # Name called by Emacs
    method: str  # Method name defined by LSP
    cancel_on_change = False  # Whether to cancel request on file change or cursor change
    send_document_uri = True
    
    def __init__(self, file_action: "FileAction"):
        self.latest_request_id = -1  # Latest request id
        self.last_change: tuple = file_action.last_change  # Last change information
        self.file_action = file_action

    def process_request(self, *args, **kwargs) -> dict:
        """Called from Emacs, return the request params."""
        raise NotImplementedError()

    def process_response(self, response: dict) -> None:
        """Process the response from LSP server."""
        raise NotImplementedError()

    def handle_response(self, request_id, response):
        if request_id != self.latest_request_id:
            logger.debug("Discard outdated response: received=%d, latest=%d",
                         request_id, self.latest_request_id)
            return

        if self.cancel_on_change and self.last_change != self.file_action.last_change:
            logger.debug("Discard response: file changed since last request")
            return

        try:
            self.process_response(response)
        except:
            logger.error("Error when processing response %d", request_id)
            import traceback
            logger.error(traceback.format_exc())

# import subclasses so that we can use core.handler.Handler.__subclasses__()
# import at the end of this file to avoid circular import
from core.handler.completion import Completion
from core.handler.completion_item import CompletionItem
from core.handler.find_define import FindDefine
from core.handler.find_implementation import FindImplementation
from core.handler.find_references import FindReferences
from core.handler.hover import Hover
from core.handler.signature_help import SignatureHelp
from core.handler.prepare_rename import PrepareRename
from core.handler.rename import Rename
from core.handler.jdt_uri_resolver import JDTUriResolver
from core.handler.code_action import CodeAction
from core.handler.formatting import Formatting
from core.handler.execute_command import ExecuteCommand
