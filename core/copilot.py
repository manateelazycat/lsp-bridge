import time
import subprocess
from core.utils import *
from subprocess import PIPE
from core.lspserver import LspServerSender, LspServerReceiver
import threading
import traceback

COPILOT_MAJOR_MODES_MAP = {
    "rustic": "rust",
    "cperl": "perl",
    "c++": "cpp",
    "objc": "objective-c",
    "cuda": "cuda-cpp",
    "docker-compose": "dockercompose",
    "coffee": "coffeescript",
    "js": "javascript",
    "js2": "javascript",
    "js2-jsx": "javascriptreact",
    "typescript-tsx": "typescriptreact",
    "rjsx": "typescriptreact",
    "less-css": "less",
    "text": "plaintext",
    "ess-r": "r",
    "enh-ruby": "ruby",
    "shell-script": "shellscript",
    "sh": "shellscript",
    "visual-basic": "vb",
    "nxml": "xml",
}

class Copilot:
    def __init__(self):
        self.is_run = False
        self.is_get_info = False

        (self.node_path, ) = get_emacs_vars(["acm-backend-copilot-node-path"])

        npm_prefix = subprocess.check_output(['npm', 'config', 'get', 'prefix'], universal_newlines=True).strip()
        self.agent_path =  os.path.join(f'{npm_prefix}/lib/node_modules', "copilot-node-server", "copilot/dist/agent.js")

        self.try_completion_timer = None
        self.file_versions = {}
        self.counter = 1
        self.wait_request = []
        self.is_get_info = False
        self.wait_id = None

    def start_copilot(self):
        self.get_info()

        if self.is_run:
            return

        self.is_run = True

        self.copilot_subprocess = subprocess.Popen([self.node_path, self.agent_path],
                                                   stdin=PIPE,
                                                   stdout=PIPE,
                                                   stderr=None)

        self.receiver = LspServerReceiver(self.copilot_subprocess, 'copilot')
        self.receiver.start()

        self.sender = LspServerSender(self.copilot_subprocess, 'copilot', 'copilot_backend')
        self.sender.start()

        self.dispatcher = threading.Thread(target=self.message_dispatcher)
        self.dispatcher.start()

        self.sender.send_request('initialize', {'capabilities': {'workspace': {'workspaceFolders': True}}}, generate_request_id(), init=True)

        self.sender.initialized.set()

        editor_info = {'editorInfo': {'name': 'Emacs', 'version': '28.0'},
                       'editorPluginInfo': {'name': 'lsp-bridge', 'version': '0.0.1'},
                       'networkProxy': epc_arg_transformer(self.proxy)}
        self.sender.send_request('setEditorInfo', editor_info, generate_request_id())

    def get_language_id(self, editor_mode):
        language_id = editor_mode.replace('-mode', '')
        return COPILOT_MAJOR_MODES_MAP[language_id] if language_id in COPILOT_MAJOR_MODES_MAP else language_id

    def accpet(self, id):
        self.sender.send_request('notifyAccepted', [{'id': id}, ], generate_request_id())

    def message_dispatcher(self):
        try:
            while True:
                message = self.receiver.get_message()
                message = message['content']

                if 'id' in message and message['id'] == self.wait_id:
                    self.wait_response = message
                    self.wait_id = None
                elif 'result' in message and 'completions' in message['result']:
                    completion_candidates = []

                    for completion in message['result']['completions']:
                        label = completion['text']
                        labels = label.strip().split("\n")
                        first_line = labels[0]

                        document = f"```{self.current_language_id}\n{label}\n```"

                        display_label = first_line
                        if len(first_line) > self.display_label_max_length:
                            display_label = "... " + display_label[len(first_line) - self.display_label_max_length:]

                        if len(labels) <= 1 and len(first_line) <= self.display_label_max_length:
                            document = ""

                        line = completion['position']['line']
                        candidate = {
                            "key": label,
                            "icon": "copilot",
                            "label": label,
                            "display-label": first_line,
                            "annotation": "Copilot",
                            "backend": "copilot",
                            "documentation": document,
                            "id": completion['uuid'],
                            "line": line,
                        }

                        completion_candidates.append(candidate)

                    eval_in_emacs("lsp-bridge-search-backend--record-items", "copilot", completion_candidates)
        except:
            logger.error(traceback.format_exc())

    def sync_file(self, text, file_path, language_id):
        if file_path in self.file_versions:
            self.sender.send_notification(
                method='textDocument/didChange',
                params={
                    'textDocument': {
                        'uri': path_to_uri(file_path),
                        'version': self.file_versions[file_path]
                    },
                    'contentChanges': [{'text': text}]
                })
        else:
            self.file_versions[file_path] = 0
            self.sender.send_notification(
                method='textDocument/didOpen',
                params={
                    'textDocument': {
                        'uri': path_to_uri(file_path),
                        'version': self.file_versions[file_path],
                        'languageId': language_id,
                        'text': text
                    }
                })

    def complete(self,  position, editor_mode, file_path, relative_path, tab_size, text, insert_spaces):
        if len(file_path) == 0:
            return

        self.start_copilot()

        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        if file_path in self.file_versions:
            self.file_versions[file_path] += 1


        self.sync_file(text, file_path, self.get_language_id(editor_mode))
        self.current_language_id = self.get_language_id(editor_mode)
        self.message = {
            "doc":
            {
                "version": self.file_versions[file_path],
                "tabSize": tab_size,
                "indentSize": tab_size,
                "insertSpaces": insert_spaces,
                "path": file_path,
                "uri": path_to_uri(file_path),
                "relativePath": relative_path,
                "languageId": self.current_language_id,
                "position": {"line": position[1], "character": position[3]},
            }
        }
        self.file_versions[file_path] += 1

        self.try_completion_timer = threading.Timer(0.0, self.do_complete)
        self.try_completion_timer.start()

    def do_complete(self):
        request_id = generate_request_id()
        self.sender.send_request(
            method='getCompletions',
            params=self.message,
            request_id=request_id
        )


    def get_info(self):
        if self.is_get_info:
            return
        (
            EMACS_VERSION,
            self.display_label_max_length,
            self.proxy,
        ) = get_emacs_vars(
            [
                "emacs-version",
                "acm-backend-codeium-candidate-max-length",
                "acm-backend-copilot-network-proxy",
            ]
        )

        self.is_get_info = True


    def login(self):
        self.start_copilot()
        self.wait_id = generate_request_id()
        self.sender.send_request('signInInitiate', {'dummy': "signInInitiate"}, self.wait_id)
        while self.wait_id is not None:
            time.sleep(0.1)
        result = self.wait_response['result']
        if result['status'] == 'AlreadySignedIn':
            message_emacs(f'Already signed in as {result["user"]}')
            return
        eval_in_emacs("browse-url", result['verificationUri'])
        message_emacs(f'Please enter user-code {result["userCode"]}')

    def logout(self):
        self.start_copilot()
        self.sender.send_request('signOut', {"dummy": "signOut"}, generate_request_id())
        message_emacs('Logged out')
