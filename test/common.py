import pprint
from typing import Any, List, Tuple

import core.utils


class EvalInterceptor:
    def __init__(self):
        self.calls = list()
        core.utils.test_interceptor = lambda method_name, args: self.calls.append((method_name, args))

    def __enter__(self) -> List[Tuple[str, List[Any]]]:
        return self.calls

    def __exit__(self, exc_type, exc_val, exc_tb):
        core.utils.logger.debug(pprint.pformat(self.calls))
        core.utils.test_interceptor = None
