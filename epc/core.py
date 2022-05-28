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


import logging
from .py3compat import SimpleXMLRPCServer

_logger = logging.getLogger(__name__)


class EPCDispatcher:

    # This class will be mixed with `SocketServer.TCPServer`,
    # which is an old style class.

    # see also: SimpleXMLRPCServer.SimpleXMLRPCDispatcher

    def __init__(self):
        self.funcs = {}
        self.instance = None

    def register_instance(self, instance, allow_dotted_names=False):
        """
        Register an instance to respond to EPC requests.

        :type instance: object
        :arg  instance:
            An object with methods to provide to peer.  If this
            instance has `_get_method` method, EPC method name
            resolution can be done by this method.

        :type allow_dotted_names: bool
        :arg  allow_dotted_names:
            If it is true, method names containing dots are supported.
            They are resolved using `getattr` for each part of the
            name as long as it does not start with '_'.

        Unlike :meth:`register_function`, only one instance can
        be registered.

        """
        self.instance = instance
        self.allow_dotted_names = allow_dotted_names

    def register_function(self, function, name=None):
        """
        Register function to be called from EPC client.

        :type  function: callable
        :arg   function: Function to publish.
        :type      name: str
        :arg       name: Name by which function is published.

        This method returns the given `function` as-is, so that you
        can use it as a decorator.

        """
        if name is None:
            name = function.__name__
        self.funcs[name] = function
        return function

    def get_method(self, name):
        """
        Get registered method callend `name`.
        """
        try:
            return self.funcs[name]
        except KeyError:
            try:
                return self.instance._get_method(name)
            except AttributeError:
                return SimpleXMLRPCServer.resolve_dotted_attribute(
                    self.instance, name, self.allow_dotted_names)


class EPCCore(EPCDispatcher):

    """
    Core methods shared by `EPCServer` and `EPCClient`.
    """

    logger = _logger

    def __init__(self, debugger, log_traceback):
        EPCDispatcher.__init__(self)
        self.set_debugger(debugger)
        self.log_traceback = log_traceback

    def set_debugger(self, debugger):
        """
        Set debugger to run when an error occurs in published method.

        You can also set debugger by passing `debugger` argument to
        the class constructor.

        :type debugger: {'pdb', 'ipdb', None}
        :arg  debugger: type of debugger.

        """
        if debugger == 'pdb':
            import pdb
            self.debugger = pdb
        elif debugger == 'ipdb':
            import ipdb
            self.debugger = ipdb
        else:
            self.debugger = debugger
