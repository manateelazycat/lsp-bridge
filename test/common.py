import os
import pprint
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, List, Tuple, Callable, Optional, NamedTuple

import core.utils
from core.utils import logger, epc_client


lsp_bridge = None

EMACS = 'emacs'

BASE_DIR = Path(__file__).resolve().parent.parent


def run_batch_sync(args, cwd=BASE_DIR):
    with run_batch(args, cwd) as p:
        for line in p.stdout:
            if "Error:" in line:
                logger.error(line)
            elif "Warning:" in line:
                logger.warn(line)
            else:
                logger.info(line)

    if p.returncode != 0:
        logger.error("Command failed with exit code %d", p.returncode)

    return p.returncode


def run_batch(args, cwd=BASE_DIR):
    init_eval = """
    (progn
        (setq network-security-level 'low) ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
        (setq package-user-dir (expand-file-name "lsp-bridge-test" temporary-file-directory))
        (require 'package)
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
        (package-initialize)
        (package-refresh-contents)
        (package-install 'markdown-mode)
        (package-install 'yasnippet)
        (package-install 'tempel)

        ;; for Windows
        (prefer-coding-system 'utf-8-unix)
        (set-language-environment 'utf-8)
        (setq default-buffer-file-coding-system 'utf-8-unix)
    )
    """

    args = [
        EMACS, "-Q", "--batch",
        '--eval', init_eval,
        "-L", os.path.join(BASE_DIR, 'acm'),
        "-L", str(BASE_DIR),
    ] + args

    logger.debug("Running command: {}".format(args))
    return subprocess.Popen(args, cwd=cwd, text=True,
                            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)


def interceptor(expectation: Callable[[str, List[Any]], bool],
                teardown: Optional[Callable[[], None]] = None,
                timeout: int = 40):
    def decorator(func):
        def wrapper(*args, **kwargs):
            ok = False
            calls = list()

            def test_interceptor(*x):
                nonlocal ok
                calls.append(x)
                ok = ok or expectation(*x)

            core.utils.test_interceptor = test_interceptor

            func(*args, **kwargs)

            tick = 0
            while not ok:
                time.sleep(1)
                tick += 1
                if tick >= timeout:
                    logger.error("timeout")
                    break

            logger.debug("Calls: %s", pprint.pformat(calls))
            core.utils.test_interceptor = None

            if teardown:
                teardown()

            assert ok
        return wrapper
    return decorator


@dataclass
class SingleFile:
    filename: str
    code: str
    mode: str


def _buffer_file_name(abspath: str) -> str:
    # Emacs only accepts "C:/a/b/c.txt" or "C:\\a\\b\\c.txt", but we have "C:\a\b\c.txt"
    if sys.platform != "win32":
        return abspath
    # same as (buffer-file-name) in Emacs under Windows
    path = Path(abspath).as_posix()
    drive = path[0]
    path = drive.lower() + path[1:]
    return path


def with_file(file: SingleFile):
    def decorator(func):
        def wrapper(*args, **kwargs):
            with tempfile.NamedTemporaryFile(delete=False, suffix=file.filename) as t_file:
                t_file.write(file.code.encode('utf-8'))
                t_file.close()

                func(*args, **kwargs, filename=_buffer_file_name(t_file.name))

                os.remove(t_file.name)
        return wrapper
    return decorator


def with_multiple_files(files: List[SingleFile]):
    def decorator(func):
        def wrapper(*args, **kwargs):
            d = dict()
            t_dir = tempfile.mkdtemp()
            os.system(f"git init --quiet {t_dir}")
            for file in files:
                path = os.path.join(t_dir, file.filename)
                os.makedirs(os.path.dirname(path), exist_ok=True)
                with open(path, "wb") as t_file:
                    t_file.write(file.code.encode('utf-8'))
                    t_file.close()
                    d[file.filename] = _buffer_file_name(t_file.name)
            func(*args, **kwargs, filenames=d)
            for name in d.values():
                os.remove(name)
        return wrapper
    return decorator


def eval_sexp_sync(sexp: str, timeout=40) -> Any:
    return epc_client.call_sync("eval-in-emacs", [sexp], timeout=timeout)


def eval_sexp(sexp: str):
    epc_client.call("eval-in-emacs", [sexp])


def file_buffer(filename: str, sexps: str) -> str:
    return f"""
    (progn
      (find-file "{filename}")
      (with-current-buffer (get-file-buffer "{filename}")
        {sexps}
      ))"""
