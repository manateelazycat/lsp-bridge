#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# NOTE
# QtWebEngine will throw error "ImportError: QtWebEngineWidgets must be imported before a QCoreApplication instance is created"
from PyQt6.QtWebEngineWidgets import QWebEngineView

from PyQt6.QtNetwork import QNetworkProxy, QNetworkProxyFactory
from PyQt6.QtWidgets import QApplication
from epc.server import ThreadingEPCServer
import os
import platform
import signal
import sys
import threading

from core.fileaction import FileAction
from core.lspserver import LspServer
from core.completionwindow import CompletionWindow
from core.utils import (PostGui, init_epc_client, close_epc_client, eval_in_emacs, get_emacs_vars, get_emacs_func_result)

class LspBridge(object):
    def __init__(self, args):
        global proxy_string
        
        self.file_action_dict = {}
        self.lsp_server_dict = {}

        for name in ["change_file", "find_define", "find_references", "prepare_rename", "rename", "change_cursor"]:
            self.build_file_action_function(name)

        self.completion_window = CompletionWindow()
            
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(lsp-bridge_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)

        self.server.register_instance(self) # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        self.get_emacs_func_result = get_emacs_func_result

        # Pass epc port and webengine codec information to Emacs when first start LspBridge.
        eval_in_emacs('lsp-bridge--first-start', [self.server.server_address[1]])

        # Disable use system proxy, avoid page slow when no network connected.
        QNetworkProxyFactory.setUseSystemConfiguration(False)

        # Set Network proxy.
        (proxy_host, proxy_port, proxy_type) = get_emacs_vars([
            "lsp-bridge-proxy-host",
            "lsp-bridge-proxy-port",
            "lsp-bridge-proxy-type"])

        self.proxy = (proxy_type, proxy_host, proxy_port)
        self.is_proxy = False

    def enable_proxy(self):
        global proxy_string

        proxy_string = "{0}://{1}:{2}".format(self.proxy[0], self.proxy[1], self.proxy[2])

        proxy = QNetworkProxy()
        if self.proxy[0] == "socks5":
            proxy.setType(QNetworkProxy.Socks5Proxy)
        elif self.proxy[0] == "http":
            proxy.setType(QNetworkProxy.HttpProxy)
        proxy.setHostName(self.proxy[1])
        proxy.setPort(int(self.proxy[2]))

        self.is_proxy = True
        QNetworkProxy.setApplicationProxy(proxy)

    def disable_proxy(self):
        global proxy_string

        proxy_string = ""

        proxy = QNetworkProxy()
        proxy.setType(QNetworkProxy.NoProxy)

        self.is_proxy = False
        QNetworkProxy.setApplicationProxy(proxy)

    def toggle_proxy(self):
        if self.is_proxy:
            self.disable_proxy()
        else:
            self.enable_proxy()
            
    @PostGui()
    def open_file(self, filepath):
        if filepath not in self.file_action_dict:
            action = FileAction(filepath)
            action.popup_completion_items.connect(self.completion_window.update_items)
            action.update_position.connect(self.completion_window.update_position)
            self.file_action_dict[filepath] = action
            
        file_action = self.file_action_dict[filepath]
        lsp_server_name = file_action.get_lsp_server_name()
        if lsp_server_name not in self.lsp_server_dict:
            server = LspServer(file_action)
            server.response_message.connect(self.handle_server_message)
            server.exit_process.connect(self.handle_server_exit)
            self.lsp_server_dict[lsp_server_name] = server
            
            self.file_action_dict[filepath].lsp_server = server
        else:
            self.lsp_server_dict[lsp_server_name].send_did_open_notification(file_action.filepath)
            
    @PostGui()
    def close_file(self, filepath):
        if filepath in self.file_action_dict:
            action = FileAction(filepath)
            
            lsp_server_name = action.get_lsp_server_name()
            if lsp_server_name in self.lsp_server_dict:
                lsp_server = self.lsp_server_dict[lsp_server_name]
                lsp_server.close_file(filepath)
                
            del self.file_action_dict[filepath]
            
    def build_file_action_function(self, name):
        @PostGui()
        def _do(*args):
            filepath = args[0]
            if filepath in self.file_action_dict:
                action = self.file_action_dict[filepath]
                getattr(action, name)(*args[1:])

        setattr(self, name, _do)
        
    @PostGui()
    def hide_completion_window(self):
        self.completion_window.hide_completion_window()

    @PostGui()
    def complete_completion_selection(self):
        self.completion_window.complete_completion_selection()

    @PostGui()
    def complete_completion_common(self):
        self.completion_window.complete_completion_common()
        
    @PostGui()
    def select_completion_next(self):
        self.completion_window.select_completion_next()

    @PostGui()
    def select_completion_previous(self):
        self.completion_window.select_completion_previous()
        
    @PostGui()
    def select_completion_last(self):
        self.completion_window.select_completion_last()

    @PostGui()
    def select_completion_first(self):
        self.completion_window.select_completion_first()
        
    def handle_server_message(self, filepath, request_type, request_id, response_result):
        if filepath in self.file_action_dict:
            self.file_action_dict[filepath].handle_response_message(request_id, request_type, response_result)
            
    def handle_server_exit(self, server_name):
        if server_name in self.lsp_server_dict:
            print("Exit server: ", server_name)
            del self.lsp_server_dict[server_name]

    def cleanup(self):
        '''Do some cleanup before exit python process.'''
        close_epc_client()

if __name__ == "__main__":
    proxy_string = ""

    destroy_view_list = []

    hardware_acceleration_args = []
    if platform.system() != "Windows":
        hardware_acceleration_args += [
            "--ignore-gpu-blocklist",
            "--enable-gpu-rasterization",
            "--enable-native-gpu-memory-buffers"]

    app = QApplication(sys.argv + ["--disable-web-security"] + hardware_acceleration_args)
    screen = app.primaryScreen()
    screen_size = screen.size()
    
    lspbridge = LspBridge(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec())
