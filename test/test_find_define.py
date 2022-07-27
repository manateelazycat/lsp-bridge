import unittest
from typing import Dict

import core.utils
from test.common import *


def get_offset(code: str, target: str):
    pos = code.find(target)
    assert pos >= 0
    return pos


class SimpleFindDefinition(unittest.TestCase):
    def jump_definition(self, file: SingleFile, cursor_offset: int, target_offset: int, filename: str):
        target_position: Optional[dict] = None

        def expectation(method: str, args: List[Any]) -> bool:
            if target_position is None:
                return False
            if method != "lsp-bridge--jump-to-def":
                return False
            return args[1] == target_position

        @interceptor(expectation)
        def go():
            nonlocal target_position
            target_position = core.utils.epc_arg_transformer(
                eval_sexp_sync(file_buffer(filename, f"""
                    (lsp-bridge--point-position (+ (point-min) {target_offset}))""")))
            start_position = core.utils.epc_arg_transformer(
                eval_sexp_sync(file_buffer(filename, f"""
                                (lsp-bridge--point-position (+ (point-min) {cursor_offset}))""")))
            logger.debug("target_position: %s", target_position)
            logger.debug("start_position: %s", start_position)
            eval_sexp(file_buffer(filename, f"""
                (setq-local major-mode '{file.mode})
                (lsp-bridge-mode 1)
                (goto-char (+ (point-min) {cursor_offset}))
                (lsp-bridge-find-define)"""))

        go()
        result_position = core.utils.epc_arg_transformer(
            eval_sexp_sync(file_buffer(filename, "(lsp-bridge--position)")))
        logger.debug("result_position: %s", result_position)
        self.assertDictEqual(target_position, result_position)

#     def test_jump_same_file(self):
#         file = SingleFile(
#             filename="test.py",
#             code="""
# for i in range(10):
#     print(i)
# """,
#             mode="python-mode"
#         )
#         cursor_offset = get_offset(file.code, "i)")
#         target_offset = get_offset(file.code, "i in")
#         with_file(file)(self.jump_definition)(file, cursor_offset, target_offset)

#     def test_jump_column_calculation(self):
#         file = SingleFile(
#             filename="test.go",
#             code="""
# package main

# import "fmt"

# func main() {
# \tfor i := 0; i < 3; i++ {
# \t\tfmt.Println(i)
# \t}
# }""",
#             mode="go-mode"
#         )
#         cursor_offset = get_offset(file.code, "i)")
#         target_offset = get_offset(file.code, "i := 0")
#         with_file(file)(self.jump_definition)(file, cursor_offset, target_offset)


class JumpOtherFile(unittest.TestCase):
    def jump(self, files: List[SingleFile], cursor_file: str, cursor_offset: int) -> str:
        result = ""

        def expectation(method: str, args: List[Any]) -> bool:
            nonlocal result
            if method == "lsp-bridge--jump-to-def":
                result = args[0]
                return True
            return False

        @with_multiple_files(files)
        @interceptor(expectation)
        def go(filenames: Dict[str, str]):
            eval_sexp(file_buffer(filenames[cursor_file], f"""
            (setq-local major-mode '{files[0].mode})
            (lsp-bridge-mode 1)
            (goto-char (+ (point-min) {cursor_offset}))
            (lsp-bridge-find-define)
            """))

        go()

        return result

    def test_jump_project(self):
        files = [
            SingleFile(
                filename="b.py",
                code="from a import *\nprint(a)\n",
                mode="python-mode",
            ),
            SingleFile(
                filename="a.py",
                code="a = 1\n",
                mode="python-mode",
            ),
        ]
        result_file = self.jump(files, "b.py", get_offset(files[0].code, "a)"))
        self.assertIn("a.py", result_file)

    def test_jump_external(self):
        files = [
            SingleFile(
                filename="b.py",
                code="import os",
                mode="python-mode",
            ),
        ]
        result_file = self.jump(files, "b.py", get_offset(files[0].code, "os"))
        self.assertIn("os", result_file)
