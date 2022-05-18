import logging
import os
import subprocess
import unittest
from pathlib import Path
from unittest import TestLoader

from core.utils import eval_in_emacs, logger, eval_sexp_in_emacs

EMACS = 'emacs'

BASE_DIR = Path(__file__).resolve().parent.parent


def run(args, cwd=BASE_DIR):
    print('Running command:', args)
    with subprocess.Popen(args, cwd=cwd,
                          stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT) as p:
        for line in p.stdout:
            print(line.decode('utf-8').rstrip())
    if p.returncode != 0:
        print('Command failed with exit code', p.returncode)
        exit(p.returncode)


def test_entrypoint():
    print('Installing dependencies...')
    init_eval = """
    (progn
        (setq package-user-dir (expand-file-name "lsp-bridge-test" temporary-file-directory))
        (require 'package)
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
        (package-initialize)
        (package-refresh-contents)
        (package-install 'corfu)
        (package-install 'all-the-icons)
        (package-install 'orderless)
        (package-install 'posframe)
        (package-install 'markdown-mode)
        ;; (all-the-icons-install-fonts 't)
    )
    """
    run([
        EMACS, '-Q', '--batch',
        '--eval', init_eval,
        '-L', '.',
        '-l', os.path.join(BASE_DIR, 'lsp-bridge.el'),
        '-l', os.path.join(BASE_DIR, 'test', 'lsp-bridge-test.el'),
        '--eval', '(lsp-bridge-start-test)'
    ])


def start_test():
    # Use only file handler in test
    logger.handlers.clear()
    logger.addHandler(logging.FileHandler(BASE_DIR / 'test.log'))

    eval_in_emacs('message', "Starting test...")

    test_loader = TestLoader()
    suite = test_loader.discover('.', pattern='test_*.py')
    logger.info(f"suite {suite}")
    test_result = unittest.TextTestRunner(verbosity=2).run(suite)

    eval_in_emacs("message", "=================== *lsp-bridge-log* ===================")
    eval_sexp_in_emacs("""
    (with-current-buffer lsp-bridge-name
      (message (buffer-string)))
    """)
    # eval_sexp_in_emacs("""
    # (with-current-buffer (get-buffer-create "*lsp-bridge-epc-log*")
    #   (message (buffer-string)))
    # """)

    if test_result.wasSuccessful():
        logger.info('All tests passed!')
        eval_in_emacs('kill-emacs')
    else:
        test_result.printErrors()
        eval_in_emacs('kill-emacs', -1)


if __name__ == '__main__':
    test_entrypoint()
