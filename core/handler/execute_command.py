from core.handler import Handler
from core.utils import *


class ExecuteCommand(Handler):
    name = "execute_command"
    method = "workspace/executeCommand"
    send_document_uri = False

    def process_request(self, server_name, command) -> dict:
        arguments = []
        
        if server_name in self.file_action.code_actions and self.file_action.code_actions[server_name] is not None:
            for action in self.file_action.code_actions[server_name]:    # type: ignore
                try:
                    if action["command"] == command:
                        arguments = action["arguments"]
                        break
                    elif action["command"]["command"] == command:
                        arguments = action["command"]["arguments"]
                        break
                except:
                    pass
        
        return dict(command=command, arguments=arguments)

    def process_response(self, response) -> None:
        pass
