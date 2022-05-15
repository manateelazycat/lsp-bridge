import os
import pprint
import tempfile
import time
from typing import Any, List, Tuple, Callable, Optional, NamedTuple

from core.utils import logger
import core.utils


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

            core.utils.logger.debug("Calls: %s", pprint.pformat(calls))
            core.utils.test_interceptor = None

            if teardown:
                teardown()

            assert ok
        return wrapper
    return decorator


class SingleFile(NamedTuple):
    filename: str
    code: str
    expectation: str
    mode: str


def with_file(file: SingleFile):
    def decorator(func):
        def wrapper(*args, **kwargs):
            with tempfile.NamedTemporaryFile(delete=False, suffix=file.filename) as t_file:
                t_file.write(file.code.encode('utf-8'))
                t_file.close()

                func(*args, **kwargs, filename=t_file.name)

                os.remove(t_file.name)
        return wrapper
    return decorator
