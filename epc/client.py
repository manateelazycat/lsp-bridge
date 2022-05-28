# Copyright (C) 2012-  Takafumi Arakaki

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from .py3compat import Queue
from .utils import ThreadedIterator, newthread
from .core import EPCCore
from .handler import ThreadingEPCHandler


class EPCClientHandler(ThreadingEPCHandler):

    # In BaseRequestHandler, everything happen in `.__init__()`.
    # Let's defer it to `.start()`.

    def __init__(self, *args):
        self._args = args
        self._ready = Queue.Queue()

    def start(self):
        ThreadingEPCHandler.__init__(self, *self._args)

    def setup(self):
        ThreadingEPCHandler.setup(self)
        self._ready.put(True)

    def wait_until_ready(self):
        self._ready.get()

    def _recv(self):
        self._recv_iter = ThreadedIterator(ThreadingEPCHandler._recv(self))
        return self._recv_iter


class EPCClient(EPCCore):

    """
    EPC client class to call remote functions and serve Python functions.

    >>> client = EPCClient()
    >>> client.connect(('localhost', 9999))                 #doctest: +SKIP
    >>> client.call_sync('echo', [111, 222, 333])           #doctest: +SKIP
    [111, 222, 333]

    To serve Python functions, you can use :meth:`register_function`.

    >>> client.register_function(str.upper)
    <method 'upper' of 'str' objects>

    :meth:`register_function` can be used as a decorator.

    >>> @client.register_function
    ... def add(x, y):
    ...     return x + y

    Also, you can initialize client and connect to the server by one line.

    >>> client = EPCClient(('localhost', 9999))             #doctest: +SKIP

    .. method:: call

       Alias of :meth:`epc.server.EPCHandler.call`.

    .. method:: call_sync

       Alias of :meth:`epc.server.EPCHandler.call_sync`.

    .. method:: methods

       Alias of :meth:`epc.server.EPCHandler.methods`.

    .. method:: methods_sync

       Alias of :meth:`epc.server.EPCHandler.methods_sync`.

    """

    thread_daemon = True

    def __init__(self, socket_or_address=None,
                 debugger=None, log_traceback=False):
        if socket_or_address is not None:
            self.connect(socket_or_address)
        EPCCore.__init__(self, debugger, log_traceback)

    def connect(self, socket_or_address):
        """
        Connect to server and start serving registered functions.

        :type socket_or_address: tuple or socket object
        :arg  socket_or_address: A ``(host, port)`` pair to be passed
                                 to `socket.create_connection`, or
                                 a socket object.

        """
        if isinstance(socket_or_address, tuple):
            import socket
            self.socket = socket.create_connection(socket_or_address)
        else:
            self.socket = socket_or_address

        # This is what BaseServer.finish_request does:
        address = None  # it is not used, so leave it empty
        self.handler = EPCClientHandler(self.socket, address, self)

        self.call = self.handler.call
        self.call_sync = self.handler.call_sync
        self.methods = self.handler.methods
        self.methods_sync = self.handler.methods_sync

        self.handler_thread = newthread(self, target=self.handler.start)
        self.handler_thread.daemon = self.thread_daemon
        self.handler_thread.start()
        self.handler.wait_until_ready()

    def close(self):
        """Close connection."""
        try:
            self.handler._recv_iter.stop()
        except AttributeError:
            # Do not fail to close even if the client is never used.
            pass

    def _ignore(*_):
        """"Do nothing method for `EPCHandler`."""
    add_client = _ignore
    remove_client = _ignore
