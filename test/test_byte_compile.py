import unittest
from glob import glob

from test.common import *

class ByteCompile(unittest.TestCase):
    def test_byte_compile(self):
        returncode = run_batch_sync(["-f", "batch-byte-compile"] + glob("acm/*.el") + glob("*.el"))
        self.assertEqual(returncode, 0, 'Byte compilation failed')
