import os
import tempfile
import time
import unittest
from typing import NamedTuple, Tuple, List, Any

from core.utils import eval_sexp_in_emacs
from test.common import EvalInterceptor


class SingleFile(NamedTuple):
    filename: str
    code: str
    expectation: str
    mode: str


def must_include_completion(e: Tuple[str, List[Any]], completion: str):
    method, args = e
    if method == "lsp-bridge-record-completion-items":
        items = args[3]
        for item in items:
            if item['label'] == completion:
                return True
    return False


def try_completion(file: SingleFile):
    t_file = tempfile.NamedTemporaryFile(delete=False, suffix=file.filename)
    t_file.write(file.code[:-1].encode('utf-8'))
    t_file.close()

    with EvalInterceptor() as calls:
        eval_sexp_in_emacs(f"""
        (progn
          (find-file "{t_file.name}")
          (with-current-buffer (get-file-buffer "{t_file.name}")
            (setq-local major-mode '{file.mode})
            (lsp-bridge-mode 1)
            (goto-char (point-max))
            (insert "{file.code[-1]}")
            ))
        """)
        time.sleep(10)
        assert any(must_include_completion(e, file.expectation) for e in calls)
    os.remove(t_file.name)


class SimpleCompletion(unittest.TestCase):
    def test_python(self):
        try_completion(SingleFile(
            filename="test.py",
            code="import os\n\nos.",
            expectation="system",
            mode="python-mode",
        ))

        try_completion(SingleFile(
            filename="test.py",
            code="import os\n\ndef 测试():\n    os.",
            expectation="system",
            mode="python-mode",
        ))

    # def test_cpp(self):
    #     try_completion(SingleFile(
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
    #     try_completion(SingleFile(
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
        try_completion(SingleFile(
            filename="test.go",
            code="""
package main

import "os"

func main() {
\tos.""",
            expectation="Open",
            mode="go-mode",
        ))
