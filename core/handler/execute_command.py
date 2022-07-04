from core.handler import Handler
from core.utils import *


class ExecuteCommand(Handler):
    name = "execute_command"
    method = "workspace/executeCommand"
    send_document_uri = False

    def process_request(self, command) -> dict:
        arguments = []
        
        if self.file_action.code_action_response != None:
            for action in self.file_action.code_action_response:    # type: ignore
                if action["command"] == command:
                    arguments = action["arguments"]
                    break
        
        return dict(command=command, arguments=arguments)

    def process_response(self, response) -> None:
        pass
