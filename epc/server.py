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


import sys
import logging

from .py3compat import SocketServer
from .utils import autolog, deprecated
from .core import EPCCore
from .handler import EPCHandler, ThreadingEPCHandler


@deprecated
def setuplogfile(logger=None, filename='python-epc.log'):
    if logger is None:
        from .core import _logger as logger
    ch = logging.FileHandler(filename=filename, mode='w')
    ch.setLevel(logging.DEBUG)
    logger.addHandler(ch)


class EPCClientManager:

    # This class will be mixed with `SocketServer.TCPServer`,
    # which is an old style class.

    def __init__(self):
        self.clients = []
        """
        A list of :class:`EPCHandler` object for connected clients.
        """

    def add_client(self, handler):
        self.clients.append(handler)
        self.handle_client_connect(handler)

    def remove_client(self, handler):
        self.clients.remove(handler)
        self.handle_client_disconnect(handler)

    def handle_client_connect(self, handler):
        """
        Handler which is called with a newly connected `client`.

        :type  handler: :class:`EPCHandler`
        :arg   handler: Object for handling request from the client.

        Default implementation does nothing.

        """

    def handle_client_disconnect(self, handler):
        """
        Handler which is called with a disconnected `client`.

        :type  handler: :class:`EPCHandler`
        :arg   handler: Object for handling request from the client.

        Default implementation does nothing.

        """


class EPCServer(SocketServer.TCPServer, EPCClientManager,
                EPCCore):

    """
    A server class to publish functions and call functions via EPC protocol.

    To publish Python functions, all you need is
    :meth:`register_function`,
    :meth:`print_port` and
    :meth:`serve_forever() <SocketServer.BaseServer.serve_forever>`.

    >>> server = EPCServer(('localhost', 0))
    >>> def echo(*a):
    ...     return a
    >>> server.register_function(echo)                 #doctest: +ELLIPSIS
    <function echo at 0x...>
    >>> server.print_port()                                #doctest: +SKIP
    9999
    >>> server.serve_forever()                             #doctest: +SKIP

    To call client's method, use :attr:`clients <EPCClientManager.clients>`
    attribute to get client handler and use its :meth:`EPCHandler.call` and
    :meth:`EPCHandler.methods` methods to communicate with connected client.

    >>> handler = server.clients[0]                        #doctest: +SKIP
    >>> def callback(reply):
    ...     print(reply)
    >>> handler.call('method_name', ['arg-1', 'arg-2', 'arg-3'],
    ...              callback)                             #doctest: +SKIP

    See :class:`SocketServer.TCPServer` and :class:`SocketServer.BaseServer`
    for other usable methods.

    """

    def __init__(self, server_address,
                 RequestHandlerClass=EPCHandler,
                 bind_and_activate=True,
                 debugger=None, log_traceback=False):
        # `BaseServer` (super class of `SocketServer`) will set
        # `RequestHandlerClass` to the attribute `self.RequestHandlerClass`.
        # This class is initialize in `BaseServer.finish_request` by
        # `self.RequestHandlerClass(request, client_address, self)`.
        SocketServer.TCPServer.__init__(
            self, server_address, RequestHandlerClass, bind_and_activate)
        EPCClientManager.__init__(self)
        EPCCore.__init__(self, debugger, log_traceback)
        self.logger.debug('-' * 75)
        self.logger.debug(
            "EPCServer is initialized: server_address = %r",
            self.server_address)

    @autolog('debug')
    def handle_error(self, request, client_address):
        self.logger.error('handle_error: trying to get traceback.format_exc')
        try:
            import traceback
            self.logger.error('handle_error: \n%s', traceback.format_exc())
        except:
            self.logger.error('handle_error: OOPS')

    def print_port(self, stream=sys.stdout):
        """
        Print port this EPC server runs on.

        As Emacs client reads port number from STDOUT, you need to
        call this just before calling :meth:`serve_forever`.

        :type stream: text stream
        :arg  stream: A stream object to write port on.
                      Default is :data:`sys.stdout`.

        """
        stream.write(str(self.server_address[1]))
        stream.write("\n")
        stream.flush()


class ThreadingEPCServer(SocketServer.ThreadingMixIn, EPCServer):

    """
    Class :class:`EPCServer` mixed with :class:`SocketServer.ThreadingMixIn`.

    Use this class when combining EPCServer with other Python module
    which has event loop, such as GUI modules.  For example, see
    `examples/gtk/server.py`_ for how to use this class with GTK

    .. _examples/gtk/server.py:
       https://github.com/tkf/python-epc/blob/master/examples/gtk/server.py

    """

    def __init__(self, *args, **kwds):
        kwds.update(RequestHandlerClass=ThreadingEPCHandler)
        EPCServer.__init__(self, *args, **kwds)


def main(args=None):
    """
    Quick CLI to serve Python functions in a module.

    Example usage::

        python -m epc.server --allow-dotted-names os

    Note that only the functions which gets and returns simple
    built-in types (str, int, float, list, tuple, dict) works.

    """
    import argparse
    from textwrap import dedent
    parser = argparse.ArgumentParser(
        formatter_class=type('EPCHelpFormatter',
                             (argparse.ArgumentDefaultsHelpFormatter,
                              argparse.RawDescriptionHelpFormatter),
                             {}),
        description=dedent(main.__doc__))
    parser.add_argument(
        'module', help='Serve python functions in this module.')
    parser.add_argument(
        '--address', default='localhost',
        help='server address')
    parser.add_argument(
        '--port', default=0, type=int,
        help='server port. 0 means to pick up random port.')
    parser.add_argument(
        '--allow-dotted-names', default=False, action='store_true')
    parser.add_argument(
        '--pdb', dest='debugger', const='pdb', action='store_const',
        help='start pdb when error occurs.')
    parser.add_argument(
        '--ipdb', dest='debugger', const='ipdb', action='store_const',
        help='start ipdb when error occurs.')
    parser.add_argument(
        '--log-traceback', action='store_true', default=False)
    ns = parser.parse_args(args)

    server = EPCServer((ns.address, ns.port),
                       debugger=ns.debugger,
                       log_traceback=ns.log_traceback)
    server.register_instance(
        __import__(ns.module),
        allow_dotted_names=ns.allow_dotted_names)
    server.print_port()
    server.serve_forever()


if __name__ == '__main__':
    main()
