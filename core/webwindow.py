#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2021 Andy Stewart
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

from PyQt6.QtGui import QColor
from PyQt6.QtCore import QUrl, Qt, QEventLoop
from PyQt6.QtWebEngineWidgets import QWebEngineView
from PyQt6.QtWebEngineCore import QWebEnginePage, QWebEngineSettings
from PyQt6.QtWidgets import QWidget, QVBoxLayout
import os
import platform

from core.utils import (get_emacs_func_result)

class BrowserPage(QWebEnginePage):
    def __init__(self):
        QWebEnginePage.__init__(self)

    def execute_javascript(self, script_src):
        ''' Execute JavaScript.'''
        # Build event loop.
        self.loop = QEventLoop()

        # Run JavaScript code.
        self.runJavaScript(script_src, self.callback_js)

        # Execute event loop, and wait event loop quit.
        self.loop.exec()

        # Return JavaScript function result.
        return self.result

    def callback_js(self, result):
        ''' Callback of JavaScript, call loop.quit to jump code after loop.exec.'''
        self.result = result
        self.loop.quit()

class WebWindow(QWidget):
    def __init__(self):
        super().__init__()
        global screen_size

        if platform.system() == "Windows":
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.Tool | Qt.WindowType.WindowDoesNotAcceptFocus)
        else:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.ToolTip)
        self.setContentsMargins(0, 0, 0, 0)

        self.vbox = QVBoxLayout(self)
        self.vbox.setContentsMargins(0, 0, 0, 0)

        self.zoom_factor = 1
        if screen_size.width() > 3000:
            self.zoom_factor = 2

        self.loading_js_code = ""
        self.load_finish_callback = None

        self.dark_mode_js = open(os.path.join(os.path.dirname(__file__), "darkreader.js")).read()

        self.update_theme_mode()

        self.webview = QWebEngineView()
        self.web_page = BrowserPage()
        self.webview.setPage(self.web_page)

        self.web_page.setBackgroundColor(QColor(get_emacs_func_result("lsp-bridge-get-theme-background", [])))

        self.webview.loadStarted.connect(lambda : self.reset_zoom())
        self.webview.loadProgress.connect(lambda : self.execute_loading_js_code())
        self.webview.loadFinished.connect(self.execute_load_finish_js_code)
        self.reset_zoom()

        self.vbox.addWidget(self.webview)
        self.setLayout(self.vbox)

        self.webview.installEventFilter(self)

        self.settings = self.webview.settings()
        try:
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.FullScreenSupportEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.DnsPrefetchEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.FocusOnNavigationEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.PlaybackRequiresUserGesture, False)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.PluginsEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.JavascriptEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.ShowScrollBars, False)
        except Exception:
            import traceback
            traceback.print_exc()

    def reset_zoom(self):
        self.webview.setZoomFactor(self.zoom_factor)

    def update_theme_mode(self):
        self.theme_mode = get_emacs_func_result("lsp-bridge-get-theme-mode", [])

    def execute_loading_js_code(self):
        if self.loading_js_code != "":
            self.webview.page().runJavaScript(self.loading_js_code)

        if self.theme_mode == "dark":
            self.load_dark_mode_js()
            self.enable_dark_mode()

    def execute_load_finish_js_code(self):
        if self.load_finish_callback != None:
            self.load_finish_callback()

    def load_dark_mode_js(self):
        self.webview.page().runJavaScript('''if (typeof DarkReader === 'undefined') {{ {} }} '''.format(self.dark_mode_js))

    def enable_dark_mode(self):
        ''' Dark mode support.'''
        self.webview.page().runJavaScript("""DarkReader.setFetchMethod(window.fetch); DarkReader.enable({brightness: 100, contrast: 90, sepia: 10});""")

    def disable_dark_mode(self):
        ''' Remove dark mode support.'''
        self.webview.page().runJavaScript("""DarkReader.disable();""")
