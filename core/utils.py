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
    if isinstance(arg, str) and arg.startswith("'"):
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
        all_paths = [i for i in paths + emacs_paths if not isinstance(i,list)]
        new_paths = list(dict.fromkeys(all_paths))

        os.environ["PATH"] = ":".join(new_paths)

        is_merge_emacs_exec_path = True

lsp_bridge_server = None
def set_lsp_bridge_server(bridge):
    global lsp_bridge_server
    lsp_bridge_server = bridge

def get_lsp_file_host():
    global lsp_bridge_server
    if lsp_bridge_server and lsp_bridge_server.file_command_server:
        return lsp_bridge_server.file_command_server.client_address[0]
    else:
        return ""

def get_buffer_content(filename, buffer_name):
    global lsp_bridge_server

    if lsp_bridge_server and lsp_bridge_server.file_server:
        return lsp_bridge_server.file_server.get_file_content(filename)
    else:
        return get_emacs_func_result('get-buffer-content', buffer_name)

def get_file_content_from_file_server(filename):
    global lsp_bridge_server

    if lsp_bridge_server and lsp_bridge_server.file_server:
        return lsp_bridge_server.file_server.get_file_content(filename)
    else:
        return ""

def get_current_line():
    return get_emacs_func_result('get-current-line')

def get_ssh_password(user, host, port):
    return get_emacs_func_result('get-ssh-password', user, host, port)

remote_connection_info = ""
def set_remote_connection_info(remote_info):
    global remote_connection_info
    remote_connection_info = remote_info

def get_remote_connection_info():
    global remote_connection_info
    return remote_connection_info

def convert_workspace_edit_path_to_tramped_path(edit, remote_connection_info):
    """ Convert documentUris in a WorkspaceEdit instance from local to remote(tramp).

        ex. 'file://...' --> 'file://ssh:...

        About WorkspaceEdit interfeface:
        https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceEdit
    """
    if "documentChanges" in edit:
        # documentChanges's item can be one of (TextDocumentEdit, CreateFile, DeleteFile, RenameFile)
        for change in edit["documentChanges"]:
            if change.get("textDocument", {}).get("uri") is not None:
                # TextDocumentEdit
                change["textDocument"]["uri"] = local_path_to_tramp_path(change["textDocument"]["uri"], remote_connection_info)
            elif "uri" in change:
                # CreateFile | DeleteFile
                change["uri"] = local_path_to_tramp_path(change["uri"], remote_connection_info)
            elif "oldUri" in change:
                # RenameFile
                change["oldUri"] = local_path_to_tramp_path(change["oldUri"], remote_connection_info)
                change["newUri"] = local_path_to_tramp_path(change["newUri"], remote_connection_info)
    elif "changes" in edit:
        changes = edit["changes"]
        new_changes = {}
        for file in changes.keys():
            tramp_file = local_path_to_tramp_path(file, remote_connection_info)
            new_changes[tramp_file] = changes[file]
        edit["changes"] = new_changes

def local_path_to_tramp_path(path, tramp_method):
    """convert path in DocumentUri format to tramp format."""
    tramp_path = path.replace("file://", "file://" + tramp_method)
    return tramp_path

def eval_in_emacs(method_name, *args):
    global lsp_bridge_server

    if test_interceptor:  # for test purpose, record all eval_in_emacs calls
        test_interceptor(method_name, args)

    args = [sexpdata.Symbol(method_name)] + list(map(handle_arg_types, args))    # type: ignore
    sexp = sexpdata.dumps(args)

    logger.debug("Eval in Emacs: %s", sexp)

    # Call eval-in-emacs elisp function.
    if lsp_bridge_server and lsp_bridge_server.file_command_server:
        lsp_bridge_server.file_command_server.send_message({
            "command": "eval-in-emacs",
            "sexp": [sexp]
        })
    else:
        eval_sexps_in_emacs([sexp])

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
    if not isinstance(arg, list):
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
    global lsp_bridge_server

    if lsp_bridge_server and lsp_bridge_server.file_elisp_server:
        return lsp_bridge_server.file_elisp_server.call_remote_rpc({
            "command": "get_emacs_vars",
            "args": args
        })
    else:
        results = epc_client.call_sync("get-emacs-vars", args)
        return list(map(lambda result: convert_emacs_bool(result[0], result[1]) if result != [] else False, results))

def get_emacs_func_result(method_name, *args):
    """Call eval-in-emacs elisp function synchronously and return the result."""
    global lsp_bridge_server

    if lsp_bridge_server and lsp_bridge_server.file_elisp_server:
        return lsp_bridge_server.file_elisp_server.call_remote_rpc({
            "command": "get_emacs_func_result",
            "method": method_name,
            "args": args
        })
    else:
        result = epc_client.call_sync(method_name, args)    # type: ignore
        return result

def get_command_result(command_string, cwd):
    import subprocess

    major, minor = sys.version_info[:2]

    if major >= 3 and minor >= 7:
        process = subprocess.Popen(command_string, cwd=cwd, shell=True, text=True,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   encoding="utf-8", errors="replace")
    else:
        process = subprocess.Popen(command_string, cwd=cwd, shell=True, universal_newlines=True,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   encoding="utf-8", errors="replace")

    ret = process.wait()
    return "".join((process.stdout if ret == 0 else process.stderr).readlines()).strip()


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

    if isinstance(project_path, str):
        return project_path
    else:
        import os
        dir_path = os.path.dirname(filepath)
        if get_command_result("git rev-parse --is-inside-work-tree", dir_path) == "true":
            path_from_git = get_command_result("git rev-parse --show-toplevel", dir_path)
            if get_os_name() == "windows":
                path_parts = path_from_git.split("/")
                # if this is a Unix-style absolute path, which should be a Windows-style one
                if path_parts[0] == "/":
                    windows_path = path_parts[1] + ":/" + "/".join(path_parts[2:])
                    return windows_path
                else:
                    return path_from_git
            else:
                return path_from_git
        else:
            return filepath

def log_time(message):
    import datetime
    logger.info("\n--- [{}] {}".format(datetime.datetime.now().time(), message))

def log_time_debug(message):
    import datetime
    logger.debug("\n--- [{}] {}".format(datetime.datetime.now().time(), message))

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

def is_valid_ip(ip):
    m = re.match(r"^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$", ip)
    return bool(m) and all(map(lambda n: 0 <= int(n) <= 255, m.groups()))

def is_valid_ip_path(ssh_path):
    """Check if SSH-PATH is a valid ssh path."""
    pattern = r"^/?(?:([a-z_][a-z0-9_\.-]*)@)?((?:[0-9]{1,3}\.){3}[0-9]{1,3})(?::(\d+))?:~?(.*)$"
    match = re.match(pattern, ssh_path)
    return match is not None

def split_ssh_path(ssh_path):
    """Split SSH-PATH into username, host, port and path."""
    pattern = r"^/?((?:([a-z_][a-z0-9_\.-]*)@)?((?:[0-9]{1,3}\.){3}[0-9]{1,3})(?::(\d+))?:?)(.*)$"
    match = re.match(pattern, ssh_path)
    if match:
        remote_info, username, host, port, path = match.groups()
        ssh_conf = {'hostname' : host}
        if username:
            ssh_conf['user'] = username
        if port:
            ssh_conf['port'] = port
        return (remote_info, host, ssh_conf, path)
    else:
        return None

def split_docker_path(docker_path):
    # Pattern to extract username, container name, and path
    pattern = r"^/docker:(?P<username>[^@]+)@(?P<container_name>[^:]+):(?P<path>/.*)$"
    match = re.match(pattern, docker_path)
    if match:
        username = match.group('username')
        container_name = match.group('container_name')
        path = match.group('path')
        return (username, container_name, path)
    else:
        return None

def is_remote_path(filepath):
    return filepath.startswith("/ssh:") or filepath.startswith("/docker:")

def eval_sexps_in_emacs(sexps: list[str]):
    if isinstance(sexps, str):
        sexps = [sexps]
        logger.warning(f"Fixed argument type. The sexp argument should be a list of sexps but got: {sexps}")
    epc_client.call("eval-in-emacs", sexps)

def string_to_base64(text):
    import base64

    base64_bytes = base64.b64encode(text.encode("utf-8"))
    base64_string = base64_bytes.decode("utf-8")

    return base64_string

def get_position(content, line, character):
    lines = content.split('\n')
    position = sum(len(lines[i]) + 1 for i in range(line)) + character
    return position

def replace_template(arg, project_path=None):
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
        return arg.replace("%USERPROFILE%", repr(windows_get_env_value("USERPROFILE")).strip("'"))
    elif "%TSDK_PATH%" in arg:
        return arg.replace("%TSDK_PATH%", repr(get_emacs_func_result("get-user-tsdk-path")).strip("'"))
    else:
        return arg

def find_csharp_solution_file(directory):
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith(".sln"):
                return os.path.join(root, file)

    return None

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

def get_value_from_path(data, path):
    """
    Retrieve a value from a nested dictionary based on the given path.
    """
    for key in path:
        if isinstance(data, dict) and key in data:
            data = data[key]
        else:
            return None
    return data

def get_nested_value(json_data, attribute_path):
    """
    Find the corresponding value in json_data based on the attribute_path.
    Handles both single paths and nested list paths.
    """
    if isinstance(attribute_path[0], list):  # Handle nested list case
        for path in attribute_path:
            value = get_value_from_path(json_data, path)
            if value is not None:
                return value
        return None
    else:
        return get_value_from_path(json_data, attribute_path)

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
