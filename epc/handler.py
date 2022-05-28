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
import itertools
import threading

from sexpdata import loads, dumps, Symbol, String

from .py3compat import SocketServer, Queue
from .utils import autolog, LockingDict, newthread, callwith


class BaseRemoteError(Exception):
    """
    All exceptions from remote method are derived from this class.
    """


class CallerUnknown(BaseRemoteError):
    """
    Error raised in remote method, but caller of the method is unknown.
    """


class EPCError(BaseRemoteError):
    """
    Error returned by `epc-error` protocol.
    """


class ReturnError(BaseRemoteError):
    """
    Error returned by `return-error` protocol.
    """


class EPCErrorCallerUnknown(CallerUnknown, EPCError):
    """
    Same as :class:`EPCError`, but caller is unknown.
    """


class ReturnErrorCallerUnknown(CallerUnknown, ReturnError):
    """
    Same as :class:`ReturnError`, but caller is unknown.
    """


class EPCClosed(Exception):
    """
    Trying to send to a closed socket.
    """


def encode_string(string):
    data = string.encode('utf-8')
    datalen = '{0:06x}'.format(len(data) + 1).encode()
    return _JOIN_BYTES([datalen, data, _NEWLINE_BYTE])
_JOIN_BYTES = ''.encode().join
_NEWLINE_BYTE = '\n'.encode()


def encode_object(obj, **kwds):
    return encode_string(dumps(obj, **kwds))


def encode_message(name, *args, **kwds):
    return encode_object([Symbol(name)] + list(args), **kwds)


def unpack_message(bytes):
    data = loads(bytes.decode('utf-8'))
    return (data[0].value(), data[1], data[2:])


def itermessage(read):
    while True:
        head = read(6)
        if not head:
            return
        length = int(head, 16)
        data = read(length)
        if len(data) < length:
            raise ValueError('need {0}-length data; got {1}'
                             .format(length, len(data)))
        yield data


class BlockingCallback(object):

    def __init__(self):
        self.queue = q = Queue.Queue()
        self.callback = lambda x: q.put(('return', x))
        self.errback = lambda x: q.put(('error', x))
        self.cbs = {'callback': self.callback, 'errback': self.errback}

    def result(self, timeout):
        (rtype, reply) = self.queue.get(timeout=timeout)
        if rtype == 'return':
            return reply
        else:
            raise reply


class EPCCallManager:

    Dict = LockingDict  # FIXME: make it configurable from server class.
    """
    Dictionary class used to store callbacks.
    """

    def __init__(self):
        self.callbacks = self.Dict()
        counter = itertools.count(1)
        self.get_uid = callwith(threading.Lock())(lambda: next(counter))
        # Wrapping by threading.Lock is useless for non-threading
        # handler.  Probably it is better to make it optional.

    def call(self, handler, name, args=[], callback=None, errback=None):
        uid = self.get_uid()
        self.callbacks[uid] = (callback, errback)
        handler._send('call', uid, Symbol(name), args)

    def methods(self, handler, callback=None, errback=None):
        uid = self.get_uid()
        self.callbacks[uid] = (callback, errback)
        handler._send('methods', uid)

    def handle_return(self, uid, reply):
        try:
            (callback, _) = self.callbacks.pop(uid)
        except (KeyError, TypeError):
            raise CallerUnknown(reply)
        if callback is not None:
            callback(reply)

    def _handle_error_reply(self, uid, reply, eclass, notfound):
        try:
            (_, errback) = self.callbacks.pop(uid)
        except (KeyError, TypeError):
            raise notfound(reply)
        error = eclass(reply)
        if errback is None:
            raise error
        else:
            errback(error)

    def handle_return_error(self, uid, reply):
        self._handle_error_reply(uid, reply, ReturnError,
                                 ReturnErrorCallerUnknown)

    def handle_epc_error(self, uid, reply):
        self._handle_error_reply(uid, reply, EPCError,
                                 EPCErrorCallerUnknown)


class EPCHandler(SocketServer.StreamRequestHandler):

    # These attribute are defined in `SocketServer.BaseRequestHandler`
    # self.server  : an instance of `EPCServer`
    # self.request :
    # self.client_address

    # These attribute are defined in `SocketServer.StreamRequestHandler`
    # self.connection : = self.request
    # self.rfile      : stream from client
    # self.wfile      : stream to client

    @property
    def logger(self):
        return self.server.logger

    @autolog('debug')
    def setup(self):
        SocketServer.StreamRequestHandler.setup(self)
        self.callmanager = EPCCallManager()
        self.server.add_client(self)

    @autolog('debug')
    def finish(self):
        try:
            SocketServer.StreamRequestHandler.finish(self)
        finally:
            self.server.remove_client(self)

    def _rfile_read_safely(self, size):
        try:
            return self.rfile.read(size)
        except (AttributeError, ValueError):
            if self.rfile.closed:
                # Calling read on closed socket raises
                # AttributeError in 2.x and ValueError in 3.x.
                # http://bugs.python.org/issue9177
                raise StopIteration
            else:
                raise  # if not, just re-raise it.

    def _recv(self):
        self.logger.debug('receiving...')
        for data in itermessage(self._rfile_read_safely):
            self.logger.debug(
                'received: length = %r; data = %r', len(data), data)
            yield data
            self.logger.debug('receiving...')

    @autolog('debug')
    def _send(self, *args):
        string = encode_message(*args)
        try:
            self.wfile.write(string)
        except (AttributeError, ValueError):
            # See also: :meth:`_rfile_read_safely`
            raise EPCClosed

    @autolog('debug')
    def handle(self):
        for sexp in self._recv():
            self._handle(sexp)

    @autolog('debug')
    def _handle(self, sexp):
        uid = undefined = []  # default: nil
        try:
            (name, uid, args) = unpack_message(sexp)
            pyname = name.replace('-', '_')
            getattr(self, '_validate_{0}'.format(pyname))(uid, args)
            handler = getattr(self, '_handle_{0}'.format(pyname))
            reply = handler(uid, *args)
            if reply is not None:
                self._send(*reply)
        except Exception as err:
            if self.handle_error(err):
                self.logger.debug(
                    'Error in handler for UID=%s (marked as handled)',
                    uid,
                    exc_info=1,
                )
                return
            if self.server.log_traceback or self.server.debugger:
                self.logger.exception('Unexpected error for UID=%s', uid)
            else:
                self.logger.error(
                    'Unexpected error for UID=%s: %s', uid, repr(err),
                )
            if self.server.debugger:
                exc_info = sys.exc_info()
                self.server.debugger.post_mortem(exc_info[2])
            name = 'epc-error' if uid is undefined else 'return-error'
            self._send(name, uid, repr(err))

    @autolog('debug')
    def _handle_call(self, uid, meth, args):
        # See: `epc:handler-called-method`
        name = meth.value()
        try:
            func = self.server.get_method(name)
        except AttributeError:
            return ['epc-error', uid,
                    "EPC-ERROR: No such method : {0}".format(name)]
        self.logger.debug('EPC handler %s: args=%s', func, args)
        result = func(*args)
        self.logger.debug('EPC handler %s: result=%s', func, result)
        return ['return', uid, result]

    def _handle_methods(self, uid):
        return ['return', uid, [
            (Symbol(name), [], String(func.__doc__ or ""))
            # FIXNE: implement arg-specs
            for (name, func)
            in self.server.funcs.items()]]

    def _handle_return(self, uid, reply):
        self.callmanager.handle_return(uid, reply)

    def _handle_return_error(self, uid, reply=None, *_):
        self.callmanager.handle_return_error(uid, reply)

    def _handle_epc_error(self, uid, reply=None, *_):
        self.callmanager.handle_epc_error(uid, reply)

    _epc_error_template = \
        "(%s %d ...): Got %s arguments in the reply: %r"

    def _validate_call(self, uid, args, num_expect=2, name='call'):
        len_args = len(args)
        if len_args == num_expect:
            return
        elif len_args < num_expect:
            message = 'Not enough arguments {0!r}'.format(args)
        else:
            message = 'Too many arguments {0!r}'.format(args)
        self._send("epc-error", uid, message)
        raise EPCError('({0} {1} ...): {2}'.format(name, uid, message))

    def _validate_methods(self, uid, args):
        self._validate_call(uid, args, 0, 'methods')

    def _validate_return(self, uid, args):
        len_args = len(args)
        error = lambda x: self._epc_error_template % ('return', uid, x, args)
        if len_args == 0:
            message = error('not enough')
        elif len_args > 1:
            message = error('too many')
        else:
            return
        self.logger.error(message)
        self._handle_epc_error(uid, message)
        raise EPCError(message)

    def _validate_return_error(self, uid, args):
        self._log_extra_argument_error('return-error', uid, args)

    def _validate_epc_error(self, uid, args):
        self._log_extra_argument_error('epc-error', uid, args)

    def _log_extra_argument_error(self, name, uid, args):
        if len(args) > 1:
            self.logger.error(self._epc_error_template,
                              'return-error', uid, 'too many', args)

    def handle_error(self, err):
        """
        Handle error which is not handled by errback.

        :type  err: Exception
        :arg   err: An error not handled by other mechanisms.
        :rtype: boolean

        Return True from this function means that error is properly
        handled, so the error is not sent to client.  Do not confuse
        this with :meth:`SocketServer.BaseServer.handle_error`.  This
        method is for handling error for each client, not for entire
        server.  Default implementation logs the error and returns
        True if the error is coming from remote [#]_ or returns False
        otherwise. Therefore, only the error occurs in this handler
        class is sent to remote.

        .. [#] More specifically, it returns True if `err` is an
           instance of :class:`BaseRemoteError` or :class:`EPCClosed`.

        """
        if isinstance(err, (BaseRemoteError, EPCClosed)):
            # BaseRemoteError: do not send error back
            # EPCClosed: no exception from thread
            return True

    def call(self, name, *args, **kwds):
        """
        Call method connected to this handler.

        :type     name: str
        :arg      name: Method name to call.
        :type     args: list
        :arg      args: Arguments for remote method to call.
        :type callback: callable
        :arg  callback: A function to be called with returned value of
                        the remote method.
        :type  errback: callable
        :arg   errback: A function to be called with an error occurred
                        in the remote method.  It is either an instance
                        of :class:`ReturnError` or :class:`EPCError`.

        """
        self.callmanager.call(self, name, *args, **kwds)

    def methods(self, *args, **kwds):
        """
        Request info of callable remote methods.

        Arguments for :meth:`call` except for `name` can be applied to
        this function too.

        """
        self.callmanager.methods(self, *args, **kwds)

    @staticmethod
    def _blocking_request(call, timeout, *args):
        bc = BlockingCallback()
        call(*args, **bc.cbs)
        return bc.result(timeout=timeout)

    def call_sync(self, name, args, timeout=None):
        """
        Blocking version of :meth:`call`.

        :type    name: str
        :arg     name: Remote function name to call.
        :type    args: list
        :arg     args: Arguments passed to the remote function.
        :type timeout: int or None
        :arg  timeout: Timeout in second.  None means no timeout.

        If the called remote function raise an exception, this method
        raise an exception.  If you give `timeout`, this method may
        raise an `Empty` exception.

        """
        return self._blocking_request(self.call, timeout, name, args)

    def methods_sync(self, timeout=None):
        """
        Blocking version of :meth:`methods`.  See also :meth:`call_sync`.
        """
        return self._blocking_request(self.methods, timeout)


class ThreadingEPCHandler(EPCHandler):

    def _handle(self, sexp):
        newthread(self, target=EPCHandler._handle, args=(self, sexp)).start()
