#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
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
import json
import logging
import os
import pathlib
import platform
import queue
import re
import subprocess
import sys
from threading import Thread
from typing import Optional
from urllib.parse import urlparse

import sexpdata
from epc.client import EPCClient

try:
    import orjson as json_parser
except:
    import json as json_parser

KIND_MAP = ["", "Text", "Method", "Function", "Constructor", "Field",
            "Variable", "Class", "Interface", "Module", "Property",
            "Unit", "Value", "Enum", "Keyword", "Snippet", "Color",
            "File", "Reference", "Folder", "EnumMember", "Constant",
            "Struct", "Event", "Operator", "TypeParameter"]

SYMBOL_MAP = ["", "File", "Module", "Namespace", "Package",
              "Class", "Method", "Property", "Field", "Constructor",
              "Enum", "Interface", "Function", "Variable", "Constant",
              "String", "Number", "Boolean", "Array", "Object",
              "Key", "Null", "EnumMember", "Struct", "Event",
              "Operator", "TypeParameter"]

epc_client: Optional[EPCClient] = None

# for test purpose
test_interceptor = None

# initialize logging, default to STDERR and INFO level
logger = logging.getLogger("lsp-bridge")
logger.setLevel(logging.INFO)
logger.addHandler(logging.StreamHandler())


def init_epc_client(emacs_server_port):
    global epc_client

    if epc_client is None:
        try:
            epc_client = EPCClient(("127.0.0.1", emacs_server_port), log_traceback=True)
        except ConnectionRefusedError:
            import traceback
            logger.error(traceback.format_exc())


def close_epc_client():
    if epc_client is not None:
        epc_client.close()


def handle_arg_types(arg):
    if type(arg) is str and arg.startswith("'"):
        arg = sexpdata.Symbol(arg.partition("'")[2])

    return sexpdata.Quoted(arg)

running_in_server = False
def set_running_in_server():
    global running_in_server

    running_in_server = True

def is_running_in_server():
    global running_in_server

    return running_in_server

is_merge_emacs_exec_path = False
def merge_emacs_exec_path():
    global is_merge_emacs_exec_path

    # Only merge once.
    if not is_merge_emacs_exec_path:
        paths = os.environ["PATH"].split(":")
        [emacs_paths, ] = get_emacs_vars(["exec-path"])
        new_paths = list(dict.fromkeys(paths + emacs_paths))

        os.environ["PATH"] = ":".join(new_paths)

        is_merge_emacs_exec_path = True

lsp_file_host = ""
def set_lsp_file_host(host):
    global lsp_file_host
    lsp_file_host = host

def get_lsp_file_host():
    global lsp_file_host
    return lsp_file_host

remote_file_server = None
def set_remote_file_server(server):
    global remote_file_server

    remote_file_server = server

def get_buffer_content(filename, buffer_name):
    global remote_file_server, lsp_file_host

    if lsp_file_host != "":
        return remote_file_server.file_dict[filename]
    else:
        return get_emacs_func_result('get-buffer-content', buffer_name)

def get_current_line():
    return get_emacs_func_result('get-current-line')

def get_ssh_password(host):
    return get_emacs_func_result('get-ssh-password', host)

remote_eval_socket = None
def set_remote_eval_socket(socket):
    global remote_eval_socket

    remote_eval_socket = socket

remote_rpc_socket = None
remote_rpc_host = None
def set_remote_rpc_socket(socket, host):
    global remote_rpc_socket
    global remote_rpc_host

    remote_rpc_socket = socket
    remote_rpc_host = host

def get_remote_rpc_socket():
    global remote_rpc_socket
    return remote_rpc_socket

def call_remote_rpc(message):
    global remote_rpc_socket, remote_rpc_host

    if remote_rpc_socket is not None:
        message["host"] = remote_rpc_host
        data = json.dumps(message)
        remote_rpc_socket.send(f"{data}\n".encode("utf-8"))

        socket_file = remote_rpc_socket.makefile("r")
        result = socket_file.readline().strip()
        socket_file.close()

        return result
    else:
        return None

def eval_in_emacs(method_name, *args):
    global remote_eval_socket

    if test_interceptor:  # for test purpose, record all eval_in_emacs calls
        test_interceptor(method_name, args)

    args = [sexpdata.Symbol(method_name)] + list(map(handle_arg_types, args))    # type: ignore
    sexp = sexpdata.dumps(args)

    logger.debug("Eval in Emacs: %s", sexp)

    # Call eval-in-emacs elisp function.
    if remote_eval_socket:
        message = {
            "command": "eval-in-emacs",
            "sexp": [sexp]
        }
        data = json.dumps(message)
        remote_eval_socket.send(f"{data}\n".encode("utf-8"))
    else:
        epc_client.call("eval-in-emacs", [sexp])    # type: ignore

def message_emacs(message: str):
    """Message to Emacs with prefix."""
    eval_in_emacs("message", "[LSP-Bridge] " + message)


def epc_arg_transformer(arg):
    """Transform elisp object to python object
    1                          => 1
    "string"                   => "string"
    (list :a 1 :b 2)           => {"a": 1, "b": 2}
    (list :a 1 :b (list :c 2)) => {"a": 1, "b": {"c": 2}}
    (list 1 2 3)               => [1 2 3]
    (list 1 2 (list 3 4))      => [1 2 [3 4]]
    """
    if type(arg) != list:
        return arg

    # NOTE: Empty list elisp can be treated as both empty python dict/list
    # Convert empty elisp list to empty python dict due to compatibility.

    # check if we can tranform arg to python dict instance
    type_dict_p = len(arg) % 2 == 0
    if type_dict_p:
        for v in arg[::2]:
            if type(v) != sexpdata.Symbol or not v.value().startswith(":"):
                type_dict_p = False
                break

    if type_dict_p:
        # transform [Symbol(":a"), 1, Symbol(":b"), 2] to dict(a=1, b=2)
        ret = dict()
        for i in range(0, len(arg), 2):
            ret[arg[i].value()[1:]] = epc_arg_transformer(arg[i + 1])
        return ret
    else:
        return list(map(epc_arg_transformer, arg))


def convert_emacs_bool(symbol_value, symbol_is_boolean):
    if symbol_is_boolean == "t":
        return symbol_value is True
    else:
        return symbol_value


def get_emacs_vars(args):
    global remote_rpc_socket

    if remote_rpc_socket:
        results = call_remote_rpc({
            "command": "get_emacs_vars",
            "args": args
        })
        return parse_json_content(results)
    else:
        results = epc_client.call_sync("get-emacs-vars", args)
        return list(map(lambda result: convert_emacs_bool(result[0], result[1]) if result != [] else False, results))

def get_emacs_func_result(method_name, *args):
    """Call eval-in-emacs elisp function synchronously and return the result."""
    global remote_rpc_socket

    if remote_rpc_socket:
        result = call_remote_rpc({
            "command": "get_emacs_func_result",
            "method": method_name,
            "args": args
        })
        return parse_json_content(result)
    else:
        result = epc_client.call_sync(method_name, args)    # type: ignore
        return result

def get_command_result(command_string, cwd):
    import subprocess

    process = subprocess.Popen(command_string, cwd=cwd, shell=True, text=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               encoding="utf-8", errors="replace")
    ret = process.wait()
    return "".join((process.stdout if ret == 0 else process.stderr).readlines()).strip()    # type: ignore


def generate_request_id():
    import random
    return abs(random.getrandbits(16))


# modified from Lib/pathlib.py
def _make_uri_win32(path):
    from urllib.parse import quote_from_bytes as urlquote_from_bytes
    # Under Windows, file URIs use the UTF-8 encoding.
    drive = path.drive
    if len(drive) == 2 and drive[1] == ':':
        # It's a path on a local drive => 'file:///c:/a/b'
        rest = path.as_posix()[2:].lstrip('/')
        return 'file:///%s%%3A/%s' % (
            drive[0], urlquote_from_bytes(rest.encode('utf-8')))
    else:
        # It's a path on a network drive => 'file://host/share/a/b'
        return 'file:' + urlquote_from_bytes(path.as_posix().encode('utf-8'))

# From jedi/api/helpers.py
def _fuzzy_match(string, like_name):
    if len(like_name) <= 1:
        return like_name in string
    pos = string.find(like_name[0])
    if pos >= 0:
        return _fuzzy_match(string[pos + 1:], like_name[1:])
    return False

def _start_match(string, like_name):
    return string.startswith(like_name)

def string_match(string, like_name, fuzzy=False):
    if fuzzy:
        return _fuzzy_match(string, like_name)
    else:
        return _start_match(string, like_name)

def path_to_uri(path):
    path = pathlib.Path(path)
    if get_os_name() != "windows":
        uri = path.as_uri()
    else:
        if not path.is_absolute():
            raise ValueError("relative path can't be expressed as a file URI")
        # encode uri to 'file:///c%3A/project/xxx.js' like vscode does
        uri = _make_uri_win32(path)
    return uri


def uri_to_path(uri):
    from urllib.parse import unquote
    # parse first, '#' may be part of filepath(encoded)
    parsed = urlparse(uri)
    # for example, ts-ls return 'file:///c%3A/lib/ref.js'
    path = unquote(parsed.path)
    if sys.platform == "win32":
        path = path[1:]
    return path


def path_as_key(path):
    key = path
    # NOTE: (buffer-file-name) return "d:/Case/a.go", gopls return "file:///D:/Case/a.go"
    if sys.platform == "win32":
        path = pathlib.Path(path).as_posix()
        key = path.lower()
    return key


def add_to_path_dict(path_dict, filepath, value):
    path_dict[path_as_key(filepath)] = value


def is_in_path_dict(path_dict, path):
    path_key = path_as_key(path)
    return path_key in path_dict


def remove_from_path_dict(path_dict, path):
    del path_dict[path_as_key(path)]


def get_from_path_dict(path_dict, filepath):
    return path_dict[path_as_key(filepath)]


def get_project_path(filepath):
    project_path = get_emacs_func_result("get-project-path", filepath)

    if type(project_path) == str:
        return project_path
    else:
        import os
        dir_path = os.path.dirname(filepath)
        if get_command_result("git rev-parse --is-inside-work-tree", dir_path) == "true":
            return get_command_result("git rev-parse --show-toplevel", dir_path)
        else:
            return filepath

def log_time(message):
    import datetime
    logger.info("\n--- [{}] {}".format(datetime.datetime.now().time(), message))

def get_os_name():
    return platform.system().lower()

def parse_json_content(content):
    return json_parser.loads(content)

def windows_get_env_value(var_name: str) -> str:
    """
    Read a Windows environment variable by os.environ and return its value.
    """
    if var_name in os.environ.keys():
        return os.environ[var_name]

def cmp(x, y):
    if x < y:
        return -1
    elif x > y:
        return 1
    else:
        return 0

def is_valid_ip_path(ssh_path):
    """Check if SSH-PATH is a valid ssh path."""
    pattern = r"^(?:([a-z_][a-z0-9_-]*)@)?((?:[0-9]{1,3}\.){3}[0-9]{1,3})(?::(\d+))?:~?(.*)$"
    match = re.match(pattern, ssh_path)
    return match is not None

def split_ssh_path(ssh_path):
    """Split SSH-PATH into username, host, port and path."""
    pattern = r"^(?:([a-z_][a-z0-9_-]*)@)?((?:[0-9]{1,3}\.){3}[0-9]{1,3})(?::(\d+))?:?(.*)$"
    match = re.match(pattern, ssh_path)
    if match:
        username, host, port, path = match.groups()
        if not username:
            username = None
        if not port:
            port = None
        return (username, host, port, path)
    else:
        return None

def eval_sexp_in_emacs(sexp):
    epc_client.call("eval-in-emacs", [sexp])

def string_to_base64(text):
    import base64

    base64_bytes = base64.b64encode(text.encode("utf-8"))
    base64_string = base64_bytes.decode("utf-8")

    return base64_string

def get_position(content, line, character):
    lines = content.split('\n')
    position = sum(len(lines[i]) + 1 for i in range(line)) + character
    return position

def replace_template(arg):
    if "%USER_EMACS_DIRECTORY%" in arg:
        if get_os_name() == "windows":
            user_emacs_dir = get_emacs_func_result("get-user-emacs-directory").replace("/", "\\")
        else:
            user_emacs_dir = get_emacs_func_result("get-user-emacs-directory")
        return arg.replace("%USER_EMACS_DIRECTORY%", user_emacs_dir)
    elif "$HOME" in arg:
            return os.path.expandvars(arg)
    elif "%FILEHASH%" in arg:
        # pyright use `--cancellationReceive` option enable "background analyze" to improve completion performance.
        return arg.replace("%FILEHASH%", os.urandom(21).hex())
    elif "%USERPROFILE%" in arg:
        return arg.replace("%USERPROFILE%", windows_get_env_value("USERPROFILE"))
    else:
        return arg

def touch(path):
    import os

    if not os.path.exists(path):
        basedir = os.path.dirname(path)

        if not os.path.exists(basedir):
            os.makedirs(basedir)

        with open(path, 'a'):
            os.utime(path)

def rebuild_content_from_diff(content, start_pos, end_pos, change_text):
    start_line = start_pos['line']
    start_char = start_pos['character']
    end_line = end_pos['line']
    end_char = end_pos['character']

    start_pos = get_position(content, start_line, start_char)
    end_pos = get_position(content, end_line, end_char)

    return content[:start_pos] + change_text + content[end_pos:]

def remove_duplicate_references(data):
    seen = set()
    result = []
    for item in data:
        t_item = (item["uri"], tuple(item["range"]["start"].items()), tuple(item["range"]["end"].items()))
        if t_item not in seen:
            seen.add(t_item)
            result.append(item)
    return result

class MessageSender(Thread):

    def __init__(self, process: subprocess.Popen):
        super().__init__()

        self.process = process
        self.queue = queue.Queue()

    def send_request(self, message):
        self.queue.put(message)

class MessageReceiver(Thread):

    def __init__(self, process: subprocess.Popen):
        super().__init__()

        self.process = process
        self.queue = queue.Queue()

    def get_message(self):
        return self.queue.get(block=True)
