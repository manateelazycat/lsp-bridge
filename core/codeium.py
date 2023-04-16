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
import random
import subprocess
import traceback
import urllib.request

from core.utils import eval_in_emacs, get_emacs_vars, get_os_name, logger, message_emacs

CODEIUM_EXECUTABLE = 'language_server.exe' if get_os_name() == 'windows' else 'language_server'


def post_request(url, data):
    json_data = json.dumps(data).encode('utf-8')

    req = urllib.request.Request(url=url, method='POST')
    req.data = json_data
    req.add_header('Content-Type', 'application/json')
    req.add_header('Content-Length', len(json_data))

    try:
        with urllib.request.urlopen(req) as response:
            response_data = response.read().decode('utf-8')

            return json.loads(response_data)
    except:
        logger.error(traceback.format_exc())
        return {}


class Codeium:
    def __init__(self):
        self.is_run = False
        self.is_get_info = False

        self.server_port = str(random.randint(40000, 49999))

    def complete(self, cursor_offset, editor_language, tab_size, text, max_num_results, insert_spaces, language):
        eval_in_emacs('lsp-bridge-search-backend--record-items', 'codeium', False)

        self.get_info()
        self.run_local_server()

        self.max_num_results = max_num_results

        data = {
            'metadata': {
                'api_key': self.api_key,
                'extension_version': self.version,
                'ide_name': 'emacs',
                'ide_version': EMACS_VERSION
            },
            'document': {
                'cursor_offset': cursor_offset,
                'editor_language': editor_language,
                'text': text,
                'language': language
            },
            'editor_options': {
                'insert_spaces': insert_spaces,
                'tab_size': tab_size
            }
        }

        self.dispatch(post_request(self.make_url('GetCompletions'), data))

    def accept(self, id):
        data = {
            'metadata': {
                'api_key': self.api_key,
                'extension_version': self.version,
                'ide_name': 'emacs',
                'ide_version': EMACS_VERSION
            },
            'completion_id': id
        }

        post_request(self.make_url('AcceptCompletion'), data)

    def get_api_key(self):
        import uuid

        self.get_info()
        self.run_local_server()

        url = 'https://codeium.com/profile?'                                 + \
              'response_type=token&'                                         + \
              'redirect_uri=http://localhost:' + self.server_port + '/auth&' + \
              'state=' + str(uuid.uuid4()) + '&'                             + \
              'scope=openid profile email&'                                  + \
              'redirect_parameters_type=query'

        eval_in_emacs('browse-url', url)

        try:
            auth_token = post_request(self.make_url('GetAuthToken'), {})['authToken']
            api_key = post_request(self.make_url('RegisterUser'), {'firebase_id_token': auth_token})['api_key']

            eval_in_emacs('customize-save-variable', "'acm-backend-codeium-api-key", api_key)
        except:
            pass

    def dispatch(self, data):
        completion_candidates = []

        if 'completionItems' in data:
            for completion in data['completionItems'][:self.max_num_results - 1]:
                label = completion['completion']['text']
                completionParts = completion.get('completionParts', [{}])[0]

                candidate = {
                    'key': label,
                    'icon': 'codeium',
                    'label': label,
                    'display-label': label.split('\n')[0].strip(),
                    'annotation': 'Codeium',
                    'backend': 'codeium',
                    'old_prefix': completionParts.get('prefix', ''),
                    'id': completion['completion']['completionId']
                }

                completion_candidates.append(candidate)

        eval_in_emacs('lsp-bridge-search-backend--record-items', 'codeium', completion_candidates)

    def make_url(self, api):
        return f'http://localhost:{self.server_port}/exa.language_server_pb.LanguageServerService/{api}'

    def run_local_server(self):
        if self.is_run:
            return

        try:
            subprocess.Popen([self.path,
                              '--api_server_host', self.api_server_host,
                              '--api_server_port', str(self.api_server_port),
                              '--server_port', self.server_port])

            self.is_run = True
        except:
            message_emacs('Cannot start codeium local server.')

    def get_info(self):
        global EMACS_VERSION

        if self.is_get_info:
            return

        [self.api_key,
         self.api_server_host,
         self.api_server_port,
         self.folder,
         self.version,
         EMACS_VERSION] = get_emacs_vars(['acm-backend-codeium-api-key',
                                          'acm-backend-codeium-api-server-host',
                                          'acm-backend-codeium-api-server-port',
                                          'codeium-bridge-folder',
                                          'codeium-bridge-binary-version',
                                          'emacs-version'])

        self.path = os.path.join(self.folder, CODEIUM_EXECUTABLE)
