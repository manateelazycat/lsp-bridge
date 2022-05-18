import pathlib
import sys
from typing import Optional
from urllib.parse import unquote, urlparse

from sexpdata import SExpBase


def is_windows():
    return sys.platform == "win32"


class Path(type(pathlib.Path()), SExpBase):
    def __init__(self, *args, **kwargs):
        pass

    def __new__(cls, *, path: Optional[str] = None, uri: Optional[str] = None):
        if uri is not None:
            # parse first, '#' may be part of filepath(encoded)
            parsed = urlparse(uri)
            # for example, ts-ls return 'file:///c%3A/lib/ref.js'
            path = unquote(parsed.path)
            if sys.platform == "win32":
                path = path[1:]
        if path is None:
            raise ValueError("path or uri is required")
        return super(Path, cls).__new__(cls, path)

    def as_path(self) -> str:
        return str(self.resolve())

    def as_uri(self) -> str:
        # modified from Lib/pathlib.py
        def make_uri_win32():
            from urllib.parse import quote_from_bytes as urlquote_from_bytes
            # Under Windows, file URIs use the UTF-8 encoding.
            drive = self.drive
            if len(drive) == 2 and drive[1] == ':':
                # It's a path on a local drive => 'file:///c:/a/b'
                rest = self.as_posix()[2:].lstrip('/')
                return 'file:///%s%%3A/%s' % (
                    drive[0], urlquote_from_bytes(rest.encode('utf-8')))
            else:
                # It's a path on a network drive => 'file://host/share/a/b'
                return 'file:' + urlquote_from_bytes(self.as_posix().encode('utf-8'))

        if is_windows():
            if not self.is_absolute():
                raise ValueError("relative path can't be expressed as a file URI")
            # encode uri to 'file:///c%3A/project/xxx.js' like vscode does
            return make_uri_win32()
        else:
            return super(Path, self).as_uri()

    def tosexp(self, tosexp):
        return tosexp(self.as_path())
