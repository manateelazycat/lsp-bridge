import os
import time
import shutil
import subprocess
import uuid
from core.utils import epc_arg_transformer, message_emacs, get_emacs_vars, get_os_name, generate_request_id, path_to_uri, eval_in_emacs, logger
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
        self.is_initialized = False
        self.is_get_info = False
        self.try_completion_timer = None
        self.file_versions = {}
        self.counter = 1
        self.wait_request = []
        self.is_get_info = False
        self.wait_id = None
        self.accept_commands = {}
        self.workspace_folders = {}

    def check_node_launch_environment(self):
        """Check if Node.js launch mode requirements are met
        Returns: (is_valid, error_message)
        """
        (node_path, ) = get_emacs_vars(["acm-backend-copilot-node-path"])

        # 1. Check node version
        try:
            version = subprocess.check_output([node_path, '-v'], stderr=subprocess.STDOUT, universal_newlines=True).strip()
            major_version = int(version.split('.')[0].lstrip('v'))
            if major_version < 20:
                return False, "Node version < 20"
        except:
            return False, "Node not found"

        # 2. Check npm and copilot-language-server package
        try:
            npm_cmd = "npm.cmd" if get_os_name() == "windows" else "npm"
            npm_package_path = subprocess.check_output([npm_cmd, 'root', '-g'], universal_newlines=True).strip()
            agent_path = os.path.join(npm_package_path, "@github/copilot-language-server/dist/language-server.js")
            if not os.path.exists(agent_path):
                return False, "@github/copilot-language-server not installed"
            self.node_path = node_path
            self.agent_path = agent_path
            return True, None
        except:
            return False, "npm not found"

    def check_binary_launch_environment(self):
        """Check if binary launch mode requirements are met
        Returns: (is_valid, error_message)
        """
        (binary_path, ) = get_emacs_vars(["acm-backend-copilot-binary-path"])

        if os.path.exists(binary_path) and os.access(binary_path, os.X_OK):
            resolved_path = binary_path
        else:
            resolved_path = shutil.which(binary_path)

        if resolved_path:
            self.binary_path = resolved_path
            return True, None
        else:
            return False, f"copilot-language-server not found or not executable: {binary_path}"

    def start_copilot(self, project_path=None):
        self.get_info()
        if self.is_run:
            return
        self.is_run = True

        (launch_mode, ) = get_emacs_vars(["acm-backend-copilot-launch-mode"])

        if str(launch_mode) == "auto":
            node_valid, node_error = self.check_node_launch_environment()
            binary_valid, binary_error = self.check_binary_launch_environment()

            if not node_valid and not binary_valid:
                message_emacs(
                    f"Copilot startup failed.\n"
                    f"Node mode: {node_error}\n"
                    f"Binary mode: {binary_error}\n"
                    f"Please either:\n"
                    f"1. Install Node.js >= 20 and npm install -g @github/copilot-language-server\n"
                    f"2. Install copilot-language-server binary and add to PATH"
                )
                self.is_run = False
                return

            if node_valid:
                self.copilot_subprocess = subprocess.Popen([self.node_path, self.agent_path, "--stdio"], stdin=PIPE, stdout=PIPE, stderr=None)
            elif binary_valid:
                self.copilot_subprocess = subprocess.Popen([self.binary_path, "--stdio"], stdin=PIPE, stdout=PIPE, stderr=None)

        elif str(launch_mode) == "node":
            is_valid, error_msg = self.check_node_launch_environment()
            if not is_valid:
                if "version" in error_msg:
                    message_emacs('To use copilot, Please install node version >= 20')
                else:
                    message_emacs('To use copilot, Please install @github/copilot-language-server')
                self.is_run = False
                return

            self.copilot_subprocess = subprocess.Popen([self.node_path, self.agent_path, "--stdio"], stdin=PIPE, stdout=PIPE, stderr=None)

        elif str(launch_mode) == "binary":
            is_valid, error_msg = self.check_binary_launch_environment()
            if not is_valid:
                message_emacs(f"Copilot binary mode failed: {error_msg}")
                self.is_run = False
                return

            self.copilot_subprocess = subprocess.Popen([self.binary_path, "--stdio"], stdin=PIPE, stdout=PIPE, stderr=None)

        self.receiver = LspServerReceiver(self.copilot_subprocess, 'copilot')
        self.receiver.start()

        self.sender = LspServerSender(self.copilot_subprocess, 'copilot', 'copilot_backend')
        self.sender.start()

        self.dispatcher = threading.Thread(target=self.message_dispatcher)
        self.dispatcher.start()

        self.wait_id = generate_request_id()
        workspace_folders = []
        if project_path is not None:
            folder = {
                "uri": path_to_uri(project_path),
                "name": os.path.basename(project_path)
            }
            workspace_folders.append(folder)
            self.workspace_folders[project_path] = folder

        self.sender.send_request('initialize', {
            'processId': os.getpid(),
            "workspaceFolders": workspace_folders,
            'clientInfo': {
                "name": "emacs",
                "version": "lsp-bridge"
            },
            'capabilities': {
                'workspace': {
                    'workspaceFolders': True,
                },
                'window': {
                    'showDocument': {
                        'support': True,
                    }
                }
            },
            'initializationOptions': {
                'editorInfo': {
                    'name': 'Emacs',
                    'version': '28.0'
                },
                'editorPluginInfo': {
                    'name': 'GitHub Copilot for lsp-bridge',
                    'version': '0.0.1'
                },
                'networkProxy': epc_arg_transformer(self.proxy)
            },
        }, self.wait_id, init=True)
        while self.wait_id is not None:
            time.sleep(0.1)
        self.sender.send_notification("initialized", {}, init=True)
        self.sender.send_notification("workspace/didChangeConfiguration", {
            'settings': {
                'telemetryLevel': 'off'
            }
        }, init=True)

        self.sender.initialized.set()

        self.is_initialized = True

    def get_language_id(self, editor_mode):
        language_id = editor_mode.replace('-mode', '')
        return COPILOT_MAJOR_MODES_MAP[language_id] if language_id in COPILOT_MAJOR_MODES_MAP else language_id

    def accept(self, id):
        self.sender.send_request('notifyAccepted', [{'id': id}, ], generate_request_id())
        if id in self.accept_commands:
            accept_command = self.accept_commands[id]
            command = accept_command['command']
            self.sender.send_request("workspace/executeCommand", {
                "command": command,
            }, generate_request_id())
            del self.accept_commands[id]

    def message_dispatcher(self):
        try:
            while True:
                message = self.receiver.get_message()
                message = message['content']

                if 'method' in message and message['method'] == 'LogMessage':
                    if message['params']['level'] > 1:
                        print('Copilot: ', message['params']['message'])
                elif 'id' in message and message['id'] == self.wait_id:
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
                            "displayLabel": first_line,
                            "annotation": "Copilot",
                            "backend": "copilot",
                            "documentation": document,
                            "id": completion['uuid'],
                            "line": line,
                        }

                        completion_candidates.append(candidate)

                    eval_in_emacs("lsp-bridge-search-backend--record-items", "copilot", completion_candidates)
                elif 'result' in message and 'items' in message['result']:

                    # inline completion response
                    completion_candidates = []

                    for item in message['result']['items']:
                        label = item['insertText']
                        labels = label.strip().split("\n")
                        first_line = labels[0]

                        document = f"```{self.current_language_id}\n{label}\n```"

                        display_label = first_line
                        if len(first_line) > self.display_label_max_length:
                            display_label = "... " + display_label[len(first_line) - self.display_label_max_length:]

                        if len(labels) <= 1 and len(first_line) <= self.display_label_max_length:
                            document = ""

                        line = item['range']['start']['line']
                        id = str(uuid.uuid4())
                        candidate = {
                            "key": label,
                            "icon": "copilot",
                            "label": label,
                            "displayLabel": first_line,
                            "annotation": "Copilot",
                            "backend": "copilot",
                            "documentation": document,
                            "id": id,
                            "line": line,
                        }
                        completion_candidates.append(candidate)
                        # accept the completion command
                        command = item['command']
                        self.accept_commands[id] = {
                            "command": command['command'],
                            "arguments": command['arguments']
                        }

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

        if not self.is_initialized:
            return

        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        if file_path in self.file_versions:
            self.file_versions[file_path] += 1

        self.did_focus(file_path)
        self.sync_file(text, file_path, self.get_language_id(editor_mode))
        self.current_language_id = self.get_language_id(editor_mode)

        # inline completions message
        self.inline_message = {
            "textDocument": {
                "uri": path_to_uri(file_path),
                "version": self.file_versions[file_path],
            },
            "position": {
                "line": position[1],
                "character": position[3]
            },
            "context": {
                "triggerKind": 2,
            },
            "formattingOptions": {
                "tabSize": tab_size,
                "insertSpaces": bool(insert_spaces),
            },
        }

        self.file_versions[file_path] += 1

        self.try_completion_timer = threading.Timer(0.0, self.do_complete)
        self.try_completion_timer.start()

    def do_complete(self):
        # send inline completion request
        self.sender.send_request(
            method='textDocument/inlineCompletion',
            params=self.inline_message,
            request_id=generate_request_id(),
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

    def check_status(self):
        self.start_copilot()
        self.wait_id = generate_request_id()
        self.sender.send_request('checkStatus', {"dummy": "checkStatus"}, self.wait_id)
        while self.wait_id is not None:
            time.sleep(0.1)
        result = self.wait_response['result']
        message_emacs(f'Copilot status: {result["status"]}' + \
                      f' as user {result["user"]}' if 'user' in result else 'NotSignedIn')

    def login(self):
        self.start_copilot()
        self.wait_id = generate_request_id()
        self.sender.send_request('signIn', {'dummy': "signInInitiate"}, self.wait_id)
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

    def change_workspace_folder(self, project_path):
        self.start_copilot(project_path=project_path)
        if not self.is_initialized:
            return
        if project_path not in self.workspace_folders:
            folder = {
                "uri": path_to_uri(project_path),
                "name": os.path.basename(project_path)
            }
            self.workspace_folders[project_path] = folder
            # notify the server
            self.sender.send_notification('workspace/didChangeWorkspaceFolders', {
                "event": {
                    "added": [folder],
                    "removed": []
                }
            })

    def did_focus(self, file_path):
        if not self.is_initialized:
            return

        self.sender.send_notification('textDocument/didFocus', {
            "textDocument": {
                "uri": path_to_uri(file_path),
            }
        })
