import unittest

from core.utils import eval_sexp_in_emacs
from test.common import *


def try_complete(file: SingleFile, label: str):
    def must_include_completion(method: str, args: List[Any]):
        if method == "lsp-bridge-record-completion-items":
            items = args[3]
            for item in items:
                if item['label'] == label:
                    return True
        return False

    @with_file(file)
    @interceptor(must_include_completion)
    def complete_file(filename: str):
        eval_sexp_in_emacs(file_buffer(filename, f"""
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
            code="import os\n\nos.",
            mode="python-mode",
        ), label="system")

    def test_python_with_utf8(self):
        try_complete(SingleFile(
            filename="test.py",
            code="import os\n\ndef 测试():\n    os.",
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
