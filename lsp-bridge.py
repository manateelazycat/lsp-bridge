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
from core.utils import (PostGui, init_epc_client, close_epc_client, eval_in_emacs, get_emacs_vars, get_emacs_func_result)

class LspBridge(object):
    def __init__(self, args):
        global proxy_string
        
        self.file_action_dict = {}
        self.lsp_server_dict = {}

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
            action.completion.connect(self.handle_completion_request)
            action.findDefine.connect(self.handle_find_define_request)
            action.findReferences.connect(self.handle_find_references_request)
            self.file_action_dict[filepath] = action
            
        file_action = self.file_action_dict[filepath]
        lsp_server_name = file_action.get_lsp_server_name()
        if lsp_server_name not in self.lsp_server_dict:
            server = LspServer(file_action)
            server.response_message.connect(self.handle_server_message)
            self.lsp_server_dict[lsp_server_name] = server
        else:
            self.lsp_server_dict[lsp_server_name].send_did_open_notification(file_action.filepath)
        
        print("Open file: ", filepath)
        
    @PostGui()
    def change_file(self, filepath, start_row, start_character, end_row, end_character, range_length, change_text, row, column, char):
        if filepath in self.file_action_dict:
            action = self.file_action_dict[filepath]
            action.change_file(row, column, char)
            
            lsp_server_name = action.get_lsp_server_name()
            if lsp_server_name in self.lsp_server_dict:
                self.lsp_server_dict[lsp_server_name].send_change_notification(
                    filepath, action.get_version(),
                    start_row, start_character, end_row, end_character, 
                    range_length, change_text, 
                    row, column, char)
            
    @PostGui()
    def find_define(self, filepath, row, column):
        if filepath in self.file_action_dict:
            action = self.file_action_dict[filepath]
            action.find_define(row, column)
    
    @PostGui()
    def find_references(self, filepath, row, column):
        if filepath in self.file_action_dict:
            action = self.file_action_dict[filepath]
            action.find_references(row, column)

    @PostGui()
    def rename(self):
        pass
    
    def handle_completion_request(self, request_id, lsp_server_name, filepath, row, column, char):
        if lsp_server_name in self.lsp_server_dict:
            self.lsp_server_dict[lsp_server_name].send_completion_request(request_id, filepath, row, column, char)

    def handle_find_define_request(self, request_id, lsp_server_name, filepath, row, column):
        if lsp_server_name in self.lsp_server_dict:
            self.lsp_server_dict[lsp_server_name].send_find_define_request(request_id, filepath, row, column)
            
    def handle_find_references_request(self, request_id, lsp_server_name, filepath, row, column):
        if lsp_server_name in self.lsp_server_dict:
            self.lsp_server_dict[lsp_server_name].send_find_references_request(request_id, filepath, row, column)
            
    def handle_server_message(self, filepath, request_type, request_id, response_result):
        if filepath in self.file_action_dict:
            action = self.file_action_dict[filepath]
            if request_type == "completion":
                if request_id == action.completion_request_list[-1]:
                    print(list(map(lambda item: item["label"], response_result["items"])))
            elif request_type == "findDefine":
                if request_id == action.find_define_request_list[-1]:
                    print(response_result)
            elif request_type == "findReferences":
                if request_id == action.find_references_request_list[-1]:
                    print(response_result)
    
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
