#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
# 
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: <lazycat.manatee@gmail.com> <lazycat.manatee@gmail.com>
# 
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

import os
import queue
import subprocess
import threading
import traceback
import json

from orjson import JSONDecodeError
from core.utils import eval_in_emacs, logger, get_os_name
from platform import version
from threading import Thread
from subprocess import PIPE
from sys import stderr

TABNINE_PROTOCOL_VERSION = "1.0.14"
TABNINE_BINARIES_FOLDER = os.path.expanduser("~/.TabNine/")
TABNINE_EXECUTABLE = "TabNine.exe" if get_os_name() == "windows" else "TabNine"

DEFAULT_BUFFER_SIZE = 100000000  # we need make buffer size big enough, avoid pipe hang by big data response from LSP server

class TabNine:
    def __init__(self):
        self.process = None
        self.path = None
        
        self.receiver = None
        self.sender = None
        self.dispatcher = None
        
        self.try_completion_timer = None

    def complete(self, before, after, filename, region_includes_beginning, region_includes_end, max_num_results):
        if self.is_tabnine_exist():
            if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
                self.try_completion_timer.cancel()
            
            self.message = {
                "version": TABNINE_PROTOCOL_VERSION,
                "request": {
                    "Autocomplete": {
                        "before": before,
                        "after": after,
                        "filename": filename,
                        "region_includes_beginning": bool(region_includes_beginning),
                        "region_includes_end": bool(region_includes_end),
                        "max_num_results": max_num_results
                    }
                }
            }
            
            self.try_completion_timer = threading.Timer(0.5, self.do_complete)
            self.try_completion_timer.start()
            
    def do_complete(self):
        self.sender.send_request(self.message)    # type: ignore
    
    def get_tabnine_path(self):
        if os.path.exists(TABNINE_BINARIES_FOLDER):
            try:
                versions = os.listdir(TABNINE_BINARIES_FOLDER)
                versions.sort(key=self.parse_version, reverse=True)
                for version in versions:
                    version_path = os.path.join(TABNINE_BINARIES_FOLDER, version)
                    if os.path.isdir(version_path):
                        distro_dir = os.listdir(version_path)[0]
                        executable_path = os.path.join(version_path, distro_dir, TABNINE_EXECUTABLE)
                        if os.path.isfile(executable_path):
                            return executable_path
            except:
                return None

        return None
    
    def is_tabnine_exist(self):
        if self.path == None:
            self.path = self.get_tabnine_path()
            
        if isinstance(self.path, str) and os.path.exists(self.path):
            if self.process == None:
                self.process = subprocess.Popen(
                    [self.path, "--client", "emacs"], 
                    bufsize=DEFAULT_BUFFER_SIZE, 
                    stdin=PIPE, 
                    stdout=PIPE, 
                    stderr=stderr)
                
                self.receiver = TabNineReceiver(self.process)
                self.receiver.start()
                
                self.sender = TabNineSender(self.process)
                self.sender.start()
                
                self.dispatcher = threading.Thread(target=self.message_dispatcher)
                self.dispatcher.start()
                
                logger.info("\nStart TabNine server{}".format(self.path))
                
            return self.process != None
        else:
            return False
        
    def message_dispatcher(self):
        try:
            while True:
                message = self.receiver.get_message()    # type: ignore
                
                completion_candidates = []
                
                if "results" in message:
                    for result in message["results"]:
                        label = result["new_prefix"]
                        
                        candidate = {
                            "key": label,
                            "icon": "tabnine",
                            "label": label,
                            "display-label": label,
                            "annotation": result["detail"] if "detail" in result else "",
                            "backend": "tabnine",
                            "new_suffix": result["new_suffix"],
                            "old_suffix": result["old_suffix"]
                        }
                        
                        completion_candidates.append(candidate)
                        
                    completion_candidates = sorted(completion_candidates, key=lambda candidate: candidate["annotation"], reverse=True)
                
                eval_in_emacs("lsp-bridge-tabnine-record-items", completion_candidates)
        except:
            logger.error(traceback.format_exc())
        
    
    def parse_version(self, s:str):
        try:
            return [int(x) for x in s.split(".")]
        except ValueError:
            return []

class TabNineSender(Thread):
    
    def __init__(self, process: subprocess.Popen):
        super().__init__()
        
        self.process = process
        self.queue = queue.Queue()
        
    def send_request(self, message):
        self.queue.put(message)
        
    def _send_message(self, message):
        data = json.dumps(message) + "\n"
        
        self.process.stdin.write(data.encode("utf-8"))    # type: ignore
        self.process.stdin.flush()    # type: ignore
        
        logger.info("\n--- Send TabNine Complete Request: {}".format(message["request"]["Autocomplete"]["filename"]))
        
        logger.debug(json.dumps(message, indent=3))
        
    def run(self):
        try:
            while self.process.poll() is None:
                message = self.queue.get()
                self._send_message(message)
        except:
            logger.error(traceback.format_exc())
    
class TabNineReceiver(Thread):
    
    def __init__(self, process: subprocess.Popen):
        super().__init__()
        
        self.process = process
        self.queue = queue.Queue()
        
    def get_message(self):
        return self.queue.get(block=True)
    
    def run(self):
        try:
            while self.process.poll() is None:
                output = self.process.stdout.readline().decode("utf-8")    # type: ignore
                try:
                    data = json.loads(output)
                    self.queue.put(data)
                except json.JSONDecodeError:
                    pass
        except:
            logger.error(traceback.format_exc())
