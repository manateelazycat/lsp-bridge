import logging
import os
import pathlib
import subprocess
import sys
import unittest
from pathlib import Path
from unittest import TestLoader

from core.utils import logger
from test import common
from test.common import eval_sexp, eval_sexp_sync

EMACS = 'emacs'

BASE_DIR = Path(__file__).resolve().parent.parent


def run(args, cwd=BASE_DIR):
    print('Running command:', args)
    with subprocess.Popen(args, cwd=cwd, text=True,
                          stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT) as p:
        for line in p.stdout:
            print(line.rstrip())
    if p.returncode != 0:
        print('Command failed with exit code', p.returncode)
        exit(p.returncode)


def test_entrypoint():
    if os.getenv("CI") is not None and sys.platform == "win32":
        tmpdir = BASE_DIR.parent / "lsp-bridge-tmp"
        tmpdir.mkdir(exist_ok=True, parents=True)
        tmppath = tmpdir.as_posix()
        os.environ["TMP"] = tmppath
        os.environ["TEMP"] = tmppath

    print('Installing dependencies...')
    init_eval = """
    (progn
        (setq network-security-level 'low) ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
        (setq package-user-dir (expand-file-name "lsp-bridge-test" temporary-file-directory))
        (require 'package)
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
        (package-initialize)
        (package-refresh-contents)
        (package-install 'posframe)
        (package-install 'markdown-mode)
        (package-install 'yasnippet)
        (package-install 'tempel)
        (package-install 'dash)
        ;; (all-the-icons-install-fonts 't)
        
        ;; for Windows
        (prefer-coding-system 'utf-8-unix)
        (set-language-environment 'utf-8)
        (setq default-buffer-file-coding-system 'utf-8-unix)
    )
    """
    setup_eval = """
    (progn
        (set-face-background 'default "#000000")
        (set-face-background 'font-lock-function-name-face "#FFFFFF")
        (require 'yasnippet)
        (require 'lsp-bridge)
        (require 'lsp-bridge-jdtls)       ;; provide Java third-party library jump and -data directory support, optional
        (require 'tempel)
        (yas-global-mode 1)
        (global-lsp-bridge-mode)
    )"""
    run([
        EMACS, '-Q', '--batch',
        '--eval', init_eval,
        '-L', '.',
        '-l', os.path.join(BASE_DIR, 'lsp-bridge.el'),
        '-l', os.path.join(BASE_DIR, 'test', 'lsp-bridge-test.el'),
        '--eval', setup_eval,
        '--eval', '(lsp-bridge-start-test)'
    ])


def start_test(lsp_bridge):
    common.lsp_bridge = lsp_bridge

    # Use only file handler in test
    logger.handlers.clear()
    logger.addHandler(logging.FileHandler(BASE_DIR / 'test.log'))

    eval_sexp_sync('(message "Starting test...")')

    test_loader = TestLoader()
    suite = test_loader.discover('.', pattern='test_*.py')
    logger.info(f"suite {suite}")
    test_result = unittest.TextTestRunner(verbosity=2).run(suite)

    eval_sexp_sync('(message "=================== *lsp-bridge-log* ===================")')
    eval_sexp_sync("""
    (with-current-buffer lsp-bridge-name
      (message (buffer-string)))
    """)

    if test_result.wasSuccessful():
        logger.info('All tests passed!')
        eval_sexp('(kill-emacs)')
    else:
        test_result.printErrors()
        eval_sexp('(kill-emacs -1)')


if __name__ == '__main__':
    test_entrypoint()
