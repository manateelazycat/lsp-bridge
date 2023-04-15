#! /usr/bin/env python
# -*- coding: utf-8 -*-

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import json
import os
import subprocess
import urllib.request

from core.utils import eval_in_emacs, get_emacs_vars, get_os_name, message_emacs

CODEIUM_EXECUTABLE = 'language_server.exe' if get_os_name() == 'windows' else 'language_server'

class Codeium:
    def __init__(self):
        self.is_run = False
        self.is_get_info = False

    def complete(self, cursor_offset, editor_language, tab_size, text, max_num_results, insert_spaces):
        eval_in_emacs('lsp-bridge-search-backend--record-items', 'codeium', False)

        if not self.is_get_info:
            self.get_info()
        if not self.is_run:
            self.run_local_server()

        self.max_num_results = max_num_results

        data = {
            'metadata': {
                'api_key': CODEIUM_API_KEY,
                'extension_version': CODEIUM_LANGUAGE_SERVER_VERSION,
                'ide_name': 'emacs',
                'ide_version': EMACS_VERSION
            },
            'document': {
                'cursor_offset': cursor_offset,
                'editor_language': editor_language,
                'text': text
            },
            'editor_options': {
                'insert_spaces': insert_spaces,
                'tab_size': tab_size
            }
        }

        self.dispatch(self.send(data))

    def dispatch(self, data):
        completion_candidates = []

        if 'completionItems' in data:
            for completion in data['completionItems'][:self.max_num_results - 1]:
                label = completion['completion']['text']
                completionParts = completion['completionParts'][0] if 'completionParts' in completion else {}

                candidate = {
                    'key': label,
                    'icon': 'codeium',
                    'label': label,
                    'display-label': label.split('\n')[0].strip(),
                    'annotation': 'Codeium',
                    'backend': 'codeium',
                    'old_prefix': completionParts['prefix'] if 'prefix' in completionParts else ''
                }

                completion_candidates.append(candidate)

        eval_in_emacs('lsp-bridge-search-backend--record-items', 'codeium', completion_candidates)

    def send(self, data):
        json_data = json.dumps(data).encode('utf-8')

        req = urllib.request.Request(url='http://localhost:42100/exa.language_server_pb.LanguageServerService/GetCompletions', method='POST')
        req.data = json_data
        req.add_header('Content-Type', 'application/json')
        req.add_header('Content-Length', len(json_data))

        try:
            response = urllib.request.urlopen(req)
            response_data = response.read().decode('utf-8')

            return json.loads(response_data)
        except:
            print('Request error')
            return {}

    def run_local_server(self):
        try:
            subprocess.Popen([self.path,
                              '--api_server_host', CODEIUM_API_SERVER_HOST,
                              '--api_server_port', str(CODEIUM_API_SERVER_PORT)])
            self.is_run = True
        except:
            message_emacs('Cannot start codeium local server.')

    def get_info(self):
        global CODEIUM_API_KEY, CODEIUM_API_SERVER_HOST, CODEIUM_API_SERVER_PORT, CODEIUM_LANGUAGE_SERVER_VERSION, CODEIUM_LOCAL_SERVER_DIR, EMACS_VERSION

        [CODEIUM_API_KEY] = get_emacs_vars(['acm-backend-codeium-api-key'])
        [CODEIUM_API_SERVER_HOST] = get_emacs_vars(['acm-backend-codeium-api-server-host'])
        [CODEIUM_API_SERVER_PORT] = get_emacs_vars(['acm-backend-codeium-api-server-port'])
        CODEIUM_LANGUAGE_SERVER_VERSION = get_emacs_vars(['codeium-bridge-binary-version'])[0].replace("language-server-v", "")
        [CODEIUM_LOCAL_SERVER_DIR] = get_emacs_vars(['codeium-bridge-folder'])
        [EMACS_VERSION] = get_emacs_vars(['emacs-version'])

        self.path = os.path.join(CODEIUM_LOCAL_SERVER_DIR, CODEIUM_EXECUTABLE)
