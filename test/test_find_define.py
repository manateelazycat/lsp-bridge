import unittest

import core.utils
from core.utils import eval_sexp_in_emacs
from test.common import *


def get_offset(code: str, target: str):
    pos = code.find(target)
    assert pos >= 0
    return pos


def jump_definition(file: SingleFile, cursor_offset: int, target_offset: int):
    emacs_position = None

    def expectation(method: str, args: List[Any]) -> bool:
        if method != "lsp-bridge--jump-to-def":
            return False
        return args[1] == emacs_position['line'] and args[2] == emacs_position['character']

    @with_file(file)
    @interceptor(expectation)
    def go(filename: str):
        nonlocal emacs_position
        emacs_position = core.utils.epc_arg_transformer(
            eval_sexp_sync(file_buffer(filename, f"""
                (lsp-bridge--point-position (+ (point-min) {target_offset}))""")))
        logger.debug("emacs_position: %s", emacs_position)
        eval_sexp_in_emacs(file_buffer(filename, f"""
            (setq-local major-mode '{file.mode})
            (lsp-bridge-mode 1)
            (goto-char (+ (point-min) {cursor_offset}))
            (lsp-bridge-find-define)"""))

    go()


class SimpleFindDefinition(unittest.TestCase):
    def test_jump_same_file(self):
        file = SingleFile(
            filename="test.py",
            code="""
for i in range(10):
    print(i)
""",
            mode="python-mode"
        )
        cursor_offset = get_offset(file.code, "i)")
        target_offset = get_offset(file.code, "i in")
        jump_definition(file, cursor_offset, target_offset)

