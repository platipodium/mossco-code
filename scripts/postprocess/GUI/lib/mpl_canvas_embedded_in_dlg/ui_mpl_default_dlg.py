# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '\\Widar\home\ak2stud\Nick\python_scripts\dev\nc_info_gui\wrk\lib\mpl_canvas_embedded_in_dlg\mpl_default_dlg.ui'
#
# Created: Thu Jul 16 14:42:14 2015
#      by: PyQt4 UI code generator 4.11.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

from mpl_canvas import mpl_widget as MatplotlibWidget


try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)



class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName(_fromUtf8("Dialog"))
        Dialog.resize(683, 599)
        Dialog.setSizeGripEnabled(True)
        self.gridLayout = QtGui.QGridLayout(Dialog)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.mplwidget = MatplotlibWidget(Dialog, grid=[1, 1], cbar=False, enable_data_cursor=False)
        self.mplwidget.setObjectName(_fromUtf8("mplwidget"))
        self.gridLayout.addWidget(self.mplwidget, 0, 0, 1, 1)

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(_translate("Dialog", "Plot", None))

