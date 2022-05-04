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

import sys,os
from PyQt6 import QtCore, QtGui, QtWidgets
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QIcon, QStandardItem, QStandardItemModel, QFont
from PyQt6.QtWidgets import (QApplication, QWidget, QListView, QLabel, QVBoxLayout)

from core.utils import get_emacs_func_result

COMPLETION_ITEM_KIND_ICON_DICT = {
    1: "text.svg",
    2: "method.svg",
    3: "function.svg",
    4: "constructor.svg",
    5: "field.svg",
    6: "variable.svg",
    7: "class.svg",
    8: "interface.svg",
    9: "module.svg",
    10: "property.svg",
    11: "unit.svg",
    12: "value.svg",
    13: "enum.svg",
    14: "keyword.svg",
    15: "snippet.svg",
    16: "color.svg",
    17: "file.svg",
    18: "reference.svg",
    19: "folder.svg",
    20: "enum_member.svg",
    21: "constant.svg",
    22: "struct.svg",
    23: "event.svg",
    24: "operator.svg",
    25: "type_parameter.svg"
}

class CompletionWindow(QWidget):

    def __init__(self):
        QWidget.__init__(self)
        
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.NoDropShadowWindowHint | Qt.WindowType.WindowTransparentForInput | Qt.WindowType.WindowDoesNotAcceptFocus | Qt.WindowType.BypassWindowManagerHint)
        self.setContentsMargins(0, 0, 0, 0)
        self.installEventFilter(self)
        
        self.dpi = int(os.environ["QT_FONT_DPI"])
        self.scale = 2 if self.dpi > 96 else 1
        
        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        
        self.listview = QListView(self) 
        self.model = QStandardItemModel()
        self.listview.setModel(self.model)
        
        self.layout.addWidget(self.listview)
        self.setLayout(self.layout)

        self.completion_items = []

    def updatePosition(self, x, y):
        self.move(int(x / self.scale), int(y / self.scale))
        self.show()
        
    def updateItems(self, completion_items):
        self.completion_items = completion_items
        
        self.model.clear()
        
        for item in completion_items:
            item = QStandardItem(
                QIcon(os.path.join(os.path.dirname(os.path.dirname(__file__)), "icons", COMPLETION_ITEM_KIND_ICON_DICT[item["type"]])), 
                item["label"])
            self.model.appendRow(item)
            
