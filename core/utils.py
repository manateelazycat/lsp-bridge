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

from epc.client import EPCClient
import base64
import sys
import pathlib
from urllib.parse import urlparse

epc_client = None

def init_epc_client(emacs_server_port):
    global epc_client

    if epc_client == None:
        try:
            epc_client = EPCClient(("localhost", emacs_server_port), log_traceback=True)
        except ConnectionRefusedError:
            import traceback
            traceback.print_exc()

def close_epc_client():
    global epc_client

    if epc_client != None:
        epc_client.close()

def string_to_base64(text):
    return str(base64.b64encode(text.encode("utf-8")), "utf-8")

def convert_arg_to_str(arg):
    if type(arg) == str:
        return arg
    elif type(arg) == list:
        arg = " ".join(map(string_to_base64, arg))
        arg = "(" + arg + ")"
        return arg
    elif type(arg) == int:
        return str(arg)
    elif type(arg) == bool:
        return str(arg).upper()
    else:
        print("Can't convert type:", type(arg))
        return str(arg)

def eval_in_emacs(method_name, args):
    global epc_client

    if epc_client == None:
        print("Please call init_epc_client first before callling eval_in_emacs.")
    else:
        # Make argument encode with Base64, avoid string quote problem pass to elisp side.
        args = list(map(convert_arg_to_str, args))
        args = list(map(string_to_base64, args))

        args.insert(0, method_name)

        # Call eval-in-emacs elisp function.
        epc_client.call("eval-in-emacs", args)

def convert_emacs_bool(symbol_value, symbol_is_boolean):
    if symbol_is_boolean == "t":
        return symbol_value == True
    else:
        return symbol_value

def get_emacs_vars(args):
    global epc_client

    return list(map(lambda result: convert_emacs_bool(result[0], result[1]) if result != [] else False, epc_client.call_sync("get-emacs-vars", args)))

def get_emacs_var(var_name):
    global epc_client

    (symbol_value, symbol_is_boolean) = epc_client.call_sync("get-emacs-var", [var_name])

    return convert_emacs_bool(symbol_value, symbol_is_boolean)

def get_emacs_func_result(method_name, args):
    global epc_client

    if epc_client == None:
        print("Please call init_epc_client first before callling eval_in_emacs.")
    else:
        # Call eval-in-emacs elisp function synchronously and return the result
        result = epc_client.call_sync(method_name, args)
        return result

def get_command_result(command_string, cwd):
    import subprocess
    return subprocess.check_output(command_string, cwd=cwd, shell=True, stderr=subprocess.STDOUT).decode("utf-8").strip()

def generate_request_id():
    import random
    return abs(random.getrandbits(16))

def path_to_uri(path):
    uri = pathlib.Path(path).as_uri()
    # from urllib.parse import quote
    # return quote(uri)
    return uri

def uri_to_path(uri):
    # from urllib.parse import unquote
    # uri = unquote(uri)
    parsed = urlparse(uri)
    path = parsed.path
    if sys.platform == "win32":
        path = parsed.path[1:]
    return path

def path_as_key(path):
    key = path
    # NOTE: (buffer-file-name) return "d:/Case/a.go", gopls return "file:///D:/Case/a.go"
    if sys.platform == "win32":
        key = path.lower()
    return key
