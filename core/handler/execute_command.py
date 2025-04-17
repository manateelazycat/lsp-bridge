from core.handler import Handler

class ExecuteCommand(Handler):
    name = "execute_command"
    method = "workspace/executeCommand"
    send_document_uri = False

    def fix_empty_objects(self, obj):
        if isinstance(obj, dict):
            # Use `null` replace `{}`, avoid "Internal error" from clojure-lsp
            if len(obj) == 0:
                return None
            return {k: self.fix_empty_objects(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [self.fix_empty_objects(item) for item in obj]
        else:
            return obj

    def process_request(self, server_name, command, arguments) -> dict:
        if server_name == "clojure-lsp":
            arguments = self.fix_empty_objects(arguments)

        return dict(command=command, arguments=arguments)

    def process_response(self, response) -> None:
        pass
