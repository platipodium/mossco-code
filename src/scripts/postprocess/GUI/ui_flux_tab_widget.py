# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '\\Widar\home\ak2stud\Nick\python_scripts\dev\nc_info_gui\wrk\lib\flux_widget\flux_tab_widget.ui'
#
# Created: Tue Aug 11 12:54:07 2015
#      by: PyQt4 UI code generator 4.11.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui




import inspect, os, sys
relative_paths = [

    '../table_view',
    '../mpl_canvas_embedded_in_dlg',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

from mpl_canvas import mpl_widget as MatplotlibWidget
from NumpyArrayTableView import NumpyArrayTableView as QTableView


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
        Dialog.resize(666, 618)
        self.gridLayout = QtGui.QGridLayout(Dialog)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.tabWidget = QtGui.QTabWidget(Dialog)
        self.tabWidget.setTabShape(QtGui.QTabWidget.Triangular)
        self.tabWidget.setElideMode(QtCore.Qt.ElideNone)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setMovable(False)
        self.tabWidget.setObjectName(_fromUtf8("tabWidget"))
        self.tab_7 = QtGui.QWidget()
        self.tab_7.setObjectName(_fromUtf8("tab_7"))
        self.gridLayout_8 = QtGui.QGridLayout(self.tab_7)
        self.gridLayout_8.setObjectName(_fromUtf8("gridLayout_8"))
        self.label = QtGui.QLabel(self.tab_7)
        self.label.setWordWrap(True)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout_8.addWidget(self.label, 0, 0, 1, 4)
        self.line = QtGui.QFrame(self.tab_7)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.gridLayout_8.addWidget(self.line, 1, 0, 1, 4)
        self.label_3 = QtGui.QLabel(self.tab_7)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout_8.addWidget(self.label_3, 2, 0, 1, 2)
        self.label_4 = QtGui.QLabel(self.tab_7)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout_8.addWidget(self.label_4, 3, 0, 1, 1)
        self.label_point1 = QtGui.QLabel(self.tab_7)
        self.label_point1.setObjectName(_fromUtf8("label_point1"))
        self.gridLayout_8.addWidget(self.label_point1, 3, 1, 1, 2)
        self.label_5 = QtGui.QLabel(self.tab_7)
        self.label_5.setObjectName(_fromUtf8("label_5"))
        self.gridLayout_8.addWidget(self.label_5, 4, 0, 1, 1)
        self.label_point2 = QtGui.QLabel(self.tab_7)
        self.label_point2.setObjectName(_fromUtf8("label_point2"))
        self.gridLayout_8.addWidget(self.label_point2, 4, 1, 1, 2)
        self.line_2 = QtGui.QFrame(self.tab_7)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName(_fromUtf8("line_2"))
        self.gridLayout_8.addWidget(self.line_2, 5, 0, 1, 4)
        self.label_2 = QtGui.QLabel(self.tab_7)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout_8.addWidget(self.label_2, 6, 0, 1, 2)
        self.lineEdit = QtGui.QLineEdit(self.tab_7)
        self.lineEdit.setObjectName(_fromUtf8("lineEdit"))
        self.gridLayout_8.addWidget(self.lineEdit, 6, 2, 1, 1)
        self.pushButton = QtGui.QPushButton(self.tab_7)
        self.pushButton.setObjectName(_fromUtf8("pushButton"))
        self.gridLayout_8.addWidget(self.pushButton, 6, 3, 1, 1)
        self.comboBox = QtGui.QComboBox(self.tab_7)
        self.comboBox.setObjectName(_fromUtf8("comboBox"))
        self.gridLayout_8.addWidget(self.comboBox, 7, 0, 1, 4)
        self.textBrowser = QtGui.QTextBrowser(self.tab_7)
        self.textBrowser.setObjectName(_fromUtf8("textBrowser"))
        self.gridLayout_8.addWidget(self.textBrowser, 8, 0, 1, 4)
        self.tabWidget.addTab(self.tab_7, _fromUtf8(""))
        self.tab = QtGui.QWidget()
        self.tab.setObjectName(_fromUtf8("tab"))
        self.gridLayout_2 = QtGui.QGridLayout(self.tab)
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))



        # user defined!!!
        self.mplwidget = MatplotlibWidget(self.tab, cbar=True)
        
        self.mplwidget.setObjectName(_fromUtf8("mplwidget"))
        self.gridLayout_2.addWidget(self.mplwidget, 0, 0, 1, 1)
        self.tabWidget.addTab(self.tab, _fromUtf8(""))
        self.tab_2 = QtGui.QWidget()
        self.tab_2.setObjectName(_fromUtf8("tab_2"))
        self.gridLayout_3 = QtGui.QGridLayout(self.tab_2)
        self.gridLayout_3.setObjectName(_fromUtf8("gridLayout_3"))
        

        self.tableView_1 = QTableView(self.tab_2)
        self.tableView_1.setObjectName(_fromUtf8("tableView_1"))
        self.gridLayout_3.addWidget(self.tableView_1, 0, 0, 1, 1)
        self.tabWidget.addTab(self.tab_2, _fromUtf8(""))
        self.tab_3 = QtGui.QWidget()
        self.tab_3.setObjectName(_fromUtf8("tab_3"))
        self.gridLayout_4 = QtGui.QGridLayout(self.tab_3)
        self.gridLayout_4.setObjectName(_fromUtf8("gridLayout_4"))
        

        self.tableView_2 = QTableView(self.tab_3)
        self.tableView_2.setObjectName(_fromUtf8("tableView_2"))
        self.gridLayout_4.addWidget(self.tableView_2, 0, 0, 1, 1)
        self.tabWidget.addTab(self.tab_3, _fromUtf8(""))
        self.tab_4 = QtGui.QWidget()
        self.tab_4.setObjectName(_fromUtf8("tab_4"))
        self.gridLayout_5 = QtGui.QGridLayout(self.tab_4)
        self.gridLayout_5.setObjectName(_fromUtf8("gridLayout_5"))
        

        self.tableView_3 = QTableView(self.tab_4)
        self.tableView_3.setObjectName(_fromUtf8("tableView_3"))
        self.gridLayout_5.addWidget(self.tableView_3, 0, 0, 1, 1)
        self.tabWidget.addTab(self.tab_4, _fromUtf8(""))
        self.tab_5 = QtGui.QWidget()
        self.tab_5.setObjectName(_fromUtf8("tab_5"))
        self.gridLayout_6 = QtGui.QGridLayout(self.tab_5)
        self.gridLayout_6.setObjectName(_fromUtf8("gridLayout_6"))
        

        self.tableView_4 = QTableView(self.tab_5)
        self.tableView_4.setObjectName(_fromUtf8("tableView_4"))
        self.gridLayout_6.addWidget(self.tableView_4, 0, 0, 1, 1)
        self.tabWidget.addTab(self.tab_5, _fromUtf8(""))
        self.tab_6 = QtGui.QWidget()
        self.tab_6.setObjectName(_fromUtf8("tab_6"))
        self.gridLayout_7 = QtGui.QGridLayout(self.tab_6)
        self.gridLayout_7.setObjectName(_fromUtf8("gridLayout_7"))
        

        self.tableView_5 = QTableView(self.tab_6)
        self.tableView_5.setObjectName(_fromUtf8("tableView_5"))
        self.gridLayout_7.addWidget(self.tableView_5, 0, 0, 1, 1)
        self.tabWidget.addTab(self.tab_6, _fromUtf8(""))
        self.gridLayout.addWidget(self.tabWidget, 0, 0, 1, 1)

        self.retranslateUi(Dialog)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(_translate("Dialog", "Dialog", None))
        self.label.setText(_translate("Dialog", "<html><head/><body><p>Here you can integrate flux over the crossection and time:<br/></p><p align=\"center\"><span style=\" font-weight:600;\">MASS = sum[ flux(x, z, t) * dx * dz] * dt</span></p><p>where:</p><p>dz, dx - length of the cells in x- and z- dimensions in meters</p><p>dt - time between timesteps in seconds</p></body></html>", None))
        self.label_3.setText(_translate("Dialog", "Section info", None))
        self.label_4.setText(_translate("Dialog", "point 1 (i,j):", None))
        self.label_point1.setText(_translate("Dialog", "?", None))
        self.label_5.setText(_translate("Dialog", "point 2 (i,j):", None))
        self.label_point2.setText(_translate("Dialog", "?", None))
        self.label_2.setText(_translate("Dialog", "Timestep range", None))
        self.pushButton.setText(_translate("Dialog", "Calculate", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_7), _translate("Dialog", "INTEGRATION", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab), _translate("Dialog", "Plot", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_2), _translate("Dialog", "Table Velocity", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_3), _translate("Dialog", "Table SPM", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_4), _translate("Dialog", "Table Cell Height", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_5), _translate("Dialog", "Table Area", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_6), _translate("Dialog", "Table Mass Flux", None))

