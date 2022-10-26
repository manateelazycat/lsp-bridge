import sys
import unittest
from typing import Dict

from test.common import *


def try_complete(file: SingleFile, label: str):
    def must_include_completion(method: str, args: List[Any]):
        if method == "lsp-bridge-completion--record-items":
            items = args[1]
            for item in items:
                if item['label'] == label:
                    return True
        return False

    @with_file(file)
    @interceptor(must_include_completion)
    def complete_file(filename: str):
        eval_sexp(file_buffer(filename, f"""
            (setq-local major-mode '{file.mode})
            (lsp-bridge-mode 1)
            (goto-char (point-max))
            (delete-char -1)
            (sleep-for 5) ;; sleep here to give lsp server enough time to compute the *complete* completion list
            (insert "{file.code[-1]}")
        """))

    return complete_file()


class SimpleCompletion(unittest.TestCase):
    def test_python(self):
        try_complete(SingleFile(
            filename="test.py",
            code="import os\n\nos.system",
            mode="python-mode",
        ), label="system")

    def test_python_with_utf8(self):
        try_complete(SingleFile(
            filename="test.py",
            code="import os\n\ndef 测试():\n    os.system",
            mode="python-mode",
        ), label="system")

    # def test_cpp(self):
    #     try_complete(SingleFile(
    #         filename="test.cpp",
    #         code="""
    #                 #include <vector>
    #
    #                 int main() {
    #                   std::vector<int> v;
    #                   v.cle""",
    #         expectation="clear()",
    #         mode="c++-mode",
    #     ))
    #     try_complete(SingleFile(
    #         filename="test.cpp",
    #         code="""
    #         #include <vector>
    #
    #         int main() {
    #           auto v = new std::vector<int>();
    #           v->cle""",
    #         expectation="clear()",
    #         mode="c++-mode",
    #     ))

    def test_go(self):
        try_complete(SingleFile(
            filename="test.go",
            code="""
package main

import "os"

func main() {
\tos.""",
            mode="go-mode",
        ), label="Open")


def get_offset(file: SingleFile, target='I'):
    pos = file.code.find(target)
    file.code = file.code.replace(target, '')
    return pos


class AcceptCompletion(unittest.TestCase):
    def setUp(self) -> None:
        if sys.platform == "win32":
            self.skipTest("To simplify dependency installation, we skip completion test on Windows")

    def accept_completion(self, files: List[SingleFile], user_input: Tuple[str, str],
                          expected: str, cursor_offset: int = None) -> None:
        """
        :param files: files of the project, we will trigger completion in the first file.
        :param user_input: a tuple of (completion trigger, user filter).
                           It will wait for completion result between completion trigger and user filter.
        :param expected: what we expect to see in the buffer after completion.
        :param cursor_offset: the offset of the cursor, if None, we will use the end of the buffer
        :return: None
        """
        if cursor_offset is None:
            cursor_offset = len(files[0].code)

        def has_completion(method: str, args: List[Any]):
            return method == "lsp-bridge-completion--record-items"

        @with_multiple_files(files)
        def go(filenames: Dict[str, str]):
            filename = filenames[files[0].filename]

            @interceptor(has_completion)
            def mock_input():

                eval_sexp(file_buffer(filename, f"""
                    (setq-local major-mode '{files[0].mode})
                    (lsp-bridge-mode 1)
                    (goto-char (+ (point-min) {cursor_offset}))
                    (insert "{user_input[0]}")
                """))

            mock_input()
            inserts = '\n'.join(
                map(lambda c: f'(execute-kbd-macro (read-kbd-macro "{c}"))\n(sleep-for 0.08)', user_input[1]))
            eval_sexp_sync(file_buffer(filename, inserts))
            eval_sexp_sync("(sleep-for 2)")
            eval_sexp_sync(file_buffer(filename, "(acm-complete)"))
            eval_sexp_sync("(sleep-for 2)")
            result = eval_sexp_sync(file_buffer(filename, "(buffer-string)"))
            logger.debug("After accepting completion: %s", result)
            self.assertIn(expected, result)

        go()
    
    # def test_python(self):
    #     self.accept_completion([SingleFile(
    #         filename="t.py",
    #         code="import os\nos",
    #         mode="python-mode",
    #     )], user_input=(".", "syste"), expected="system")

    # def test_java(self):
    #     file = SingleFile(
    #         filename="t.java",
    #         code="""
    #         public class Test {
    #             public static void main(String[] args) {
    #                 System.out.I
    #             }
    #         }
    #         """,
    #         mode="java-mode",
    #     )
    #     self.accept_completion(
    #         [file],
    #         user_input=("printl", "nString"),
    #         expected="println();\n",
    #         cursor_offset=get_offset(file),
    #     )

#     def test_volar(self):
#         files = [SingleFile(
#             filename=os.path.join("src", "t.vue"),
#             code="""
# <template>
#     <div></div>
# </template>
# <script lang="ts">
# interface Test {
#   member?: [];
# }
#
# const test: Test = {
#   member: []
# };
#
# test.member.filI
# </script>""",
#             mode="vue-mode",
#         )]
#         self.accept_completion(
#             files,
#             user_input=("te", "r"),
#             expected="member?.filter",
#             cursor_offset=get_offset(files[0])
#         )
