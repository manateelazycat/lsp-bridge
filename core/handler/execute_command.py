from core.handler import Handler
from core.utils import *


class ExecuteCommand(Handler):
    name = "execute_command"
    method = "workspace/executeCommand"
    send_document_uri = False

    def process_request(self, server_name, command, arguments) -> dict:
        return dict(command=command, arguments=arguments)

    def process_response(self, response) -> None:
        pass
