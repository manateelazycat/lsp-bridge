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
import functools
import logging
import platform
from typing import Optional

import sexpdata
from epc.client import EPCClient

from core.path import Path

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
            epc_client = EPCClient(("localhost", emacs_server_port), log_traceback=True)
        except ConnectionRefusedError:
            import traceback
            logger.error(traceback.format_exc())


def close_epc_client():
    if epc_client is not None:
        epc_client.close()


def eval_sexp_in_emacs(sexp: str, **kwargs):
    logger.debug("Eval in Emacs: %s", sexp)
    # Call eval-in-emacs elisp function.
    epc_client.call("eval-in-emacs", [sexp], **kwargs)


def eval_in_emacs(method_name, *args, no_intercept=False, **kwargs):
    if test_interceptor and not no_intercept:  # for test purpose, record all eval_in_emacs calls
        test_interceptor(method_name, args)

    args = [sexpdata.Symbol(method_name)] + list(map(sexpdata.Quoted, args))
    sexp = sexpdata.dumps(args)
    logger.debug("Eval in Emacs: %s", sexp)
    # Call eval-in-emacs elisp function.
    epc_client.call("eval-in-emacs", [sexp], **kwargs)


def message_emacs(message: str):
    """Message to Emacs with prefix."""
    eval_in_emacs("message", "[LSP-Bridge] " + message)


def epc_arg_transformer(arg):
    """Transform [Symbol(":a"), 1, Symbol(":b"), 2] to dict(a=1, b=2)"""
    if type(arg) != list or len(arg) % 2 != 0:
        return arg

    for i, v in enumerate(arg):
        if i % 2 == 1:
            continue
        if type(v) != sexpdata.Symbol or not v.value().startswith(":"):
            return arg

    ret = dict()
    for i in range(0, len(arg), 2):
        ret[arg[i].value()[1:]] = arg[i + 1]
    return ret


def convert_emacs_bool(symbol_value, symbol_is_boolean):
    if symbol_is_boolean == "t":
        return symbol_value is True
    else:
        return symbol_value


def get_emacs_vars(args):
    return list(map(lambda result: convert_emacs_bool(result[0], result[1]) if result != [] else False,
                    epc_client.call_sync("get-emacs-vars", args)))


def get_emacs_var(var_name):
    symbol_value, symbol_is_boolean = epc_client.call_sync("get-emacs-var", [var_name])

    return convert_emacs_bool(symbol_value, symbol_is_boolean)


def get_emacs_func_result(method_name, *args):
    """Call eval-in-emacs elisp function synchronously and return the result."""
    result = epc_client.call_sync(method_name, args)
    return result


def get_command_result(command_string, cwd: Path):
    import subprocess
    process = subprocess.Popen(command_string, cwd=cwd, shell=True, text=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               encoding="utf-8")
    ret = process.wait()
    return "".join((process.stdout if ret == 0 else process.stderr).readlines()).strip()


def generate_request_id():
    import random
    return abs(random.getrandbits(16))


def get_project_path(path: Path) -> Path:
    dir_path = path.parent
    if get_command_result("git rev-parse --is-inside-work-tree", dir_path) == "true":
        return Path(path=get_command_result("git rev-parse --show-toplevel", dir_path))
    else:
        return path


@functools.lru_cache(maxsize=None)
def get_emacs_version():
    return get_emacs_func_result("get-emacs-version")


def get_os_name():
    return platform.system().lower()
