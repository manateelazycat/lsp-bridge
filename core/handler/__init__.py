import abc
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from core.fileaction import FileAction
from core.utils import *


class Handler(abc.ABC):
    name = "NAME"
    method = "METHOD"
    cancel_on_change = False

    def __init__(self, fa: "FileAction"):
        self.latest_request_id = -1
        self.last_change = fa.last_change
        self.fa = fa

    def process_request(self, *args, **kwargs) -> dict:
        raise NotImplementedError()

    def process_response(self, response: dict) -> None:
        raise NotImplementedError()

    def send_request(self, *args, **kwargs):
        self.latest_request_id = request_id = generate_request_id()
        self.last_change = self.fa.last_change

        self.fa.lsp_server.record_request_id(
            request_id=request_id,
            method=self.method,
            filepath=self.fa.filepath,
            type=self.name,
        )

        params = self.process_request(*args, **kwargs)
        params["textDocument"] = {
            "uri": self.fa.lsp_server.filepath_uri_dict[self.fa.filepath]
        }

        self.fa.lsp_server.send_to_request(
            name=self.method,
            params=params,
            request_id=request_id,
        )

    def handle_response(self, request_id, response):
        if request_id != self.latest_request_id:
            logger.debug("Discard out-to-date response: received=%d, latest=%d",
                         request_id, self.latest_request_id)
            return

        if self.cancel_on_change and self.last_change != self.fa.last_change:
            logger.debug("Discard response: file changed since last request")
            return

        try:
            self.process_response(response)
        except:
            logger.error("Error when processing response %d", request_id)
            import traceback
            logger.error(traceback.format_exc())

# prevent circular import
from core.handler.completion import Completion
from core.handler.find_define import FindDefine
from core.handler.find_implementation import FindImplementation
from core.handler.find_references import FindReferences
from core.handler.hover import Hover
from core.handler.signature_help import SignatureHelp
from core.handler.prepare_rename import PrepareRename
from core.handler.rename import Rename
from core.handler.jdt_class_contents import JDTClassContents
