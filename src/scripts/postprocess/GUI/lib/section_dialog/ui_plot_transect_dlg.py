# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '\\Widar\home\ak2stud\Nick\python_scripts\dev\nc_info_gui\lib\plot_transect_dlg.ui'
#
# Created: Mon Jul 13 16:06:58 2015
#      by: PyQt4 UI code generator 4.11.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

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
        Dialog.resize(350, 450)
        Dialog.setMinimumSize(QtCore.QSize(350, 450))
        Dialog.setModal(True)
        self.gridLayout_5 = QtGui.QGridLayout(Dialog)
        self.gridLayout_5.setObjectName(_fromUtf8("gridLayout_5"))
        self.label_3 = QtGui.QLabel(Dialog)
        self.label_3.setMinimumSize(QtCore.QSize(300, 0))
        self.label_3.setWordWrap(True)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout_5.addWidget(self.label_3, 0, 0, 1, 1)
        self.groupBox_p1 = QtGui.QGroupBox(Dialog)
        self.groupBox_p1.setMinimumSize(QtCore.QSize(300, 80))
        self.groupBox_p1.setObjectName(_fromUtf8("groupBox_p1"))
        self.gridLayout = QtGui.QGridLayout(self.groupBox_p1)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.label = QtGui.QLabel(self.groupBox_p1)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout.addWidget(self.label, 0, 0, 1, 1)
        self.lineEdit_lat1 = QtGui.QLineEdit(self.groupBox_p1)
        self.lineEdit_lat1.setMinimumSize(QtCore.QSize(0, 20))
        self.lineEdit_lat1.setObjectName(_fromUtf8("lineEdit_lat1"))
        self.gridLayout.addWidget(self.lineEdit_lat1, 0, 1, 1, 1)
        self.label_2 = QtGui.QLabel(self.groupBox_p1)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 0, 2, 1, 1)
        self.lineEdit_lon1 = QtGui.QLineEdit(self.groupBox_p1)
        self.lineEdit_lon1.setMinimumSize(QtCore.QSize(0, 20))
        self.lineEdit_lon1.setText(_fromUtf8(""))
        self.lineEdit_lon1.setObjectName(_fromUtf8("lineEdit_lon1"))
        self.gridLayout.addWidget(self.lineEdit_lon1, 0, 3, 1, 1)
        self.pushButton_pick1 = QtGui.QPushButton(self.groupBox_p1)
        self.pushButton_pick1.setMinimumSize(QtCore.QSize(50, 0))
        self.pushButton_pick1.setMaximumSize(QtCore.QSize(50, 16777215))
        self.pushButton_pick1.setObjectName(_fromUtf8("pushButton_pick1"))
        self.gridLayout.addWidget(self.pushButton_pick1, 0, 4, 1, 1)
        self.gridLayout_5.addWidget(self.groupBox_p1, 1, 0, 1, 1)
        self.groupBox = QtGui.QGroupBox(Dialog)
        self.groupBox.setMinimumSize(QtCore.QSize(300, 80))
        self.groupBox.setObjectName(_fromUtf8("groupBox"))
        self.gridLayout_2 = QtGui.QGridLayout(self.groupBox)
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))
        self.label_4 = QtGui.QLabel(self.groupBox)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout_2.addWidget(self.label_4, 0, 0, 1, 1)
        self.lineEdit_lat2 = QtGui.QLineEdit(self.groupBox)
        self.lineEdit_lat2.setMinimumSize(QtCore.QSize(0, 20))
        self.lineEdit_lat2.setObjectName(_fromUtf8("lineEdit_lat2"))
        self.gridLayout_2.addWidget(self.lineEdit_lat2, 0, 1, 1, 1)
        self.label_5 = QtGui.QLabel(self.groupBox)
        self.label_5.setObjectName(_fromUtf8("label_5"))
        self.gridLayout_2.addWidget(self.label_5, 0, 2, 1, 1)
        self.lineEdit_lon2 = QtGui.QLineEdit(self.groupBox)
        self.lineEdit_lon2.setMinimumSize(QtCore.QSize(0, 20))
        self.lineEdit_lon2.setText(_fromUtf8(""))
        self.lineEdit_lon2.setObjectName(_fromUtf8("lineEdit_lon2"))
        self.gridLayout_2.addWidget(self.lineEdit_lon2, 0, 3, 1, 1)
        self.pushButton_pick2 = QtGui.QPushButton(self.groupBox)
        self.pushButton_pick2.setMinimumSize(QtCore.QSize(50, 0))
        self.pushButton_pick2.setMaximumSize(QtCore.QSize(50, 16777215))
        self.pushButton_pick2.setObjectName(_fromUtf8("pushButton_pick2"))
        self.gridLayout_2.addWidget(self.pushButton_pick2, 0, 4, 1, 1)
        self.gridLayout_5.addWidget(self.groupBox, 2, 0, 1, 1)
        self.groupBox_4 = QtGui.QGroupBox(Dialog)
        self.groupBox_4.setMinimumSize(QtCore.QSize(300, 80))
        self.groupBox_4.setObjectName(_fromUtf8("groupBox_4"))
        self.gridLayout_3 = QtGui.QGridLayout(self.groupBox_4)
        self.gridLayout_3.setObjectName(_fromUtf8("gridLayout_3"))
        self.label_6 = QtGui.QLabel(self.groupBox_4)
        self.label_6.setObjectName(_fromUtf8("label_6"))
        self.gridLayout_3.addWidget(self.label_6, 0, 0, 1, 1)
        self.lineEdit_nVector = QtGui.QLineEdit(self.groupBox_4)
        self.lineEdit_nVector.setMinimumSize(QtCore.QSize(0, 20))
        self.lineEdit_nVector.setMaximumSize(QtCore.QSize(50, 16777215))
        self.lineEdit_nVector.setToolTip(_fromUtf8(""))
        self.lineEdit_nVector.setObjectName(_fromUtf8("lineEdit_nVector"))
        self.gridLayout_3.addWidget(self.lineEdit_nVector, 0, 1, 1, 1)
        spacerItem = QtGui.QSpacerItem(207, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.gridLayout_3.addItem(spacerItem, 0, 2, 1, 1)
        self.gridLayout_5.addWidget(self.groupBox_4, 3, 0, 1, 1)
        self.groupBox_2 = QtGui.QGroupBox(Dialog)
        self.groupBox_2.setMinimumSize(QtCore.QSize(300, 80))
        self.groupBox_2.setObjectName(_fromUtf8("groupBox_2"))
        self.gridLayout_4 = QtGui.QGridLayout(self.groupBox_2)
        self.gridLayout_4.setObjectName(_fromUtf8("gridLayout_4"))
        self.lineEdit_bathymetry = QtGui.QLineEdit(self.groupBox_2)
        self.lineEdit_bathymetry.setMinimumSize(QtCore.QSize(0, 20))
        self.lineEdit_bathymetry.setObjectName(_fromUtf8("lineEdit_bathymetry"))
        self.gridLayout_4.addWidget(self.lineEdit_bathymetry, 0, 0, 1, 1)
        self.pushButton_selectBathymetry = QtGui.QPushButton(self.groupBox_2)
        self.pushButton_selectBathymetry.setMinimumSize(QtCore.QSize(50, 0))
        self.pushButton_selectBathymetry.setMaximumSize(QtCore.QSize(50, 16777215))
        self.pushButton_selectBathymetry.setObjectName(_fromUtf8("pushButton_selectBathymetry"))
        self.gridLayout_4.addWidget(self.pushButton_selectBathymetry, 0, 1, 1, 1)
        self.gridLayout_5.addWidget(self.groupBox_2, 4, 0, 1, 1)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.gridLayout_5.addWidget(self.buttonBox, 5, 0, 1, 1)

        self.retranslateUi(Dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(_translate("Dialog", "Plot_GF_transect_coord", None))
        self.label_3.setText(_translate("Dialog", "This dialog is a launcher for a script written by onur.kerimoglu@hzg.de. Enter coordinates in decimal degrees (or pick from map) of two desired points to generate crossection. Select netcdf file with bathymetry data. The currently selected in main widget variable will be used as a data-source for this crosssection-plot.", None))
        self.groupBox_p1.setTitle(_translate("Dialog", "Point 1", None))
        self.label.setText(_translate("Dialog", "Lat", None))
        self.label_2.setText(_translate("Dialog", "Lon", None))
        self.pushButton_pick1.setText(_translate("Dialog", "Pick", None))
        self.groupBox.setTitle(_translate("Dialog", "Point 2", None))
        self.label_4.setText(_translate("Dialog", "Lat", None))
        self.label_5.setText(_translate("Dialog", "Lon", None))
        self.pushButton_pick2.setText(_translate("Dialog", "Pick", None))
        self.groupBox_4.setTitle(_translate("Dialog", "Transect-Length", None))
        self.label_6.setText(_translate("Dialog", "N-points", None))
        self.groupBox_2.setTitle(_translate("Dialog", "Bathymetry file", None))
        self.pushButton_selectBathymetry.setText(_translate("Dialog", "Select", None))

