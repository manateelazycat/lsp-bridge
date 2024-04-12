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
import re
import subprocess
import tempfile
import time
import traceback
import urllib.parse
import urllib.request

from core.utils import *

CODEIUM_EXECUTABLE = (
    "language_server.exe" if get_os_name() == "windows" else "language_server"
)


class Codeium:
    def __init__(self):
        self.is_run = False
        self.is_get_info = False

        (self.api_key_path,) = get_emacs_vars(["acm-backend-codeium-api-key-path"])

        self.server_port = ""
        self.current_cussor_offset = 0

        self.counter = 1
        self.wait_request = []

    def complete(
        self,
        cursor_offset,
        editor_language,
        tab_size,
        text,
        insert_spaces,
        prefix,
        language,
    ):
        self.get_info()
        self.run_local_server()

        # utf-8 cursor offset
        cursor_offset = len(text[:cursor_offset].encode("utf-8", errors="ignore"))
        self.current_cussor_offset = cursor_offset

        for _ in self.wait_request:
            self.metadata["request_id"] = self.wait_request.pop()
            self.post_request(
                self.make_url("CancelRequest"), {"metadata": self.metadata}
            )

        self.metadata["request_id"] = self.counter
        self.wait_request.append(self.counter)
        self.counter += 1

        data = {
            "metadata": self.metadata,
            "document": {
                "cursor_offset": cursor_offset,
                "editor_language": editor_language,
                "text": text,
                "language": language,
            },
            "editor_options": {"insert_spaces": insert_spaces, "tab_size": tab_size},
        }

        self.dispatch(
            self.post_request(self.make_url("GetCompletions"), data),
            editor_language,
            prefix,
            cursor_offset,
        )

    def accept(self, id):
        data = {"metadata": self.metadata, "completion_id": id}

        self.post_request(self.make_url("AcceptCompletion"), data)

    def auth(self):
        import uuid

        params = {
            "response_type": "token",
            "redirect_uri": "vim-show-auth-token",
            "state": str(uuid.uuid4()),
            "scope": "openid profile email",
            "redirect_parameters_type": "query",
        }

        url = "https://codeium.com/profile?" + urllib.parse.urlencode(params)

        eval_in_emacs("browse-url", url)

    def get_api_key(self, auth_token):
        message_emacs("Getting api key...")

        api_key = self.post_request(
            "https://api.codeium.com/register_user/", {"firebase_id_token": auth_token}
        )["api_key"]

        # Save API key in configure file.
        touch(self.api_key_path)
        with open(self.api_key_path, "w") as f:
            f.write(api_key)

        message_emacs(f"Has save codeium API Key at {self.api_key_path}.")

        self.is_get_info = False
        self.get_info()

    def dispatch(self, data, editor_language, prefix, cursor_offset=None):
        if self.current_cussor_offset != cursor_offset:
            # drop old completion items
            return

        completion_candidates = []

        current_line = get_current_line()

        if "completionItems" in data:
            language = editor_language.split("-")[0]
            language = language.replace("emacs", "elisp")

            for completion in data["completionItems"][: self.max_num_results - 1]:
                label = completion["completion"]["text"]
                labels = label.strip().split("\n")
                first_line = labels[0]

                document = f"```{language}\n{label}\n```"

                # Don't make display label bigger than max length.
                display_label = first_line
                if len(first_line) > self.display_label_max_length:
                    display_label = "... " + display_label[len(first_line) - self.display_label_max_length:]

                # Only hide documentation when label smaller than max length and only 1 line
                if len(labels) <= 1 and len(first_line) <= self.display_label_max_length:
                        document = ""

                completion_parts = completion.get("completionParts", [{}])[0]
                annotation = (
                    "Codeium"
                    if "prefix" in completion_parts or current_line == ""
                    else "Replace"
                )

                if completion_parts.get("type") == "COMPLETION_PART_TYPE_BLOCK":
                    annotation = "Replace"

                if label == current_line:
                    continue

                candidate = {
                    "key": label,
                    "icon": "codeium",
                    "label": label,
                    "displayLabel": display_label,
                    "annotation": annotation,
                    "backend": "codeium",
                    "documentation": document,
                    "id": completion["completion"]["completionId"],
                    "line": int(completion_parts.get("line", 0)),
                }

                completion_candidates.append(candidate)

        eval_in_emacs(
            "lsp-bridge-search-backend--record-items", "codeium", completion_candidates
        )

    def make_url(self, api):
        return f"http://127.0.0.1:{self.server_port}/exa.language_server_pb.LanguageServerService/{api}"

    def run_local_server(self):
        if self.is_run:
            return

        try:
            self.is_run = True

            message_emacs("Waiting for Codeium local server to start...")

            self.manager_dir = tempfile.mkdtemp(prefix="codeium_")
            params = [self.path, "--manager_dir", self.manager_dir]

            params += [
                "--api_server_url",
                f"https://{self.api_server_host}:{str(self.api_server_port)}",
            ]

            process = subprocess.Popen(params)

            self.get_server_port()
        except:
            self.is_run = False

            process.kill()

            logger.error(traceback.format_exc())
            message_emacs("Cannot start codeium local server.")

    def get_info(self):
        if self.is_get_info:
            return

        (
            EMACS_VERSION,
            self.VERSION,
            self.api_server_host,
            self.api_server_port,
            self.folder,
            self.max_num_results,
            self.display_label_max_length,
        ) = get_emacs_vars(
            [
                "emacs-version",
                "codeium-bridge-binary-version",
                "acm-backend-codeium-api-server-host",
                "acm-backend-codeium-api-server-port",
                "codeium-bridge-folder",
                "acm-backend-codeium-candidates-number",
                "acm-backend-codeium-candidate-max-length",
            ]
        )

        # Try read API_KEY from config file.
        API_KEY = ""
        if os.path.exists(self.api_key_path):
            with open(self.api_key_path, "r") as f:
                API_KEY = f.read().strip()

        self.metadata = {
            "api_key": API_KEY,
            "extension_version": self.VERSION,
            "ide_name": "emacs",
            "ide_version": EMACS_VERSION,
        }
        self.path = os.path.join(self.folder, CODEIUM_EXECUTABLE)
        self.is_get_info = True

    def get_server_port(self):
        pattern = re.compile(r"\d{5}")

        while True:
            try:
                files = [f for f in os.listdir(self.manager_dir) if pattern.match(f)]

                time.sleep(0.1)

                if len(files) == 0:
                    continue
                else:
                    break
            except:
                pass

        if len(files) > 0:
            self.server_port = files[0]
        else:
            time.sleep(0.1)
            self.get_server_port()

    def post_request(self, url, data):
        json_data = json.dumps(data).encode("utf-8")

        req = urllib.request.Request(url=url, method="POST")
        req.data = json_data
        req.add_header("Content-Type", "application/json")
        req.add_header("Content-Length", len(json_data))

        try:
            with urllib.request.urlopen(req) as response:
                response_data = response.read().decode("utf-8")
                return parse_json_content(response_data)
        except Exception as e:
            logger.exception(e)
            return {}
