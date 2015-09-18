#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# -----------------------------------------------------------------------------
# > @brief NCINFO - Visualization tool for NOSSCO-output files
#
#   This computer program is part of MOSSCO.
# > @copyright Copyright (C) 2013, 2014, 2015 Bundesanstalt fuer Wasserbau
# > @author Nikolai Chernikov, <nikolai.chernikov.ru@gmail.com>
#
#  MOSSCO is free software: you can redistribute it and/or modify it under the
#  terms of the GNU General Public License v3+.  MOSSCO is distributed in the
#  hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
#  LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
# ------------------------------------------------------------------------------

import os, sys
from PyQt4 import QtGui

# use this if you want to include modules from a subfolder
import inspect
relative_paths = [
    'lib/main_dialog',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

from dataselect import selectdata_dlg

__version__ = 1.0




class MainWindow(QtGui.QMainWindow):
    
    def __init__(self):
        super(MainWindow, self).__init__()
        
        self.initUI()
        
    

    def initUI(self):
        self.main_widget = selectdata_dlg(path=os.path.dirname(os.path.abspath(sys.argv[0])))
        self.setCentralWidget(self.main_widget)

        self.statusBar()
       
        self.initMenu()
        self.setWindowTitle('nc_info')
        self.center()
        self.show()
    

    def center(self):
        qr = self.frameGeometry()
        cp = QtGui.QDesktopWidget().availableGeometry().center()
        qr.moveCenter(cp)
        self.move(qr.topLeft())


    def initMenu(self):

        exitAction = QtGui.QAction(QtGui.QIcon.fromTheme('application-exit'), 'Exit', self)
        exitAction.setShortcut('Alt+F4')
        exitAction.setStatusTip('Exit application')
        exitAction.triggered.connect(self.close)


        reloadSettingsAction = QtGui.QAction(QtGui.QIcon.fromTheme('view-refresh'), 'Reload Settings', self)
        reloadSettingsAction.setShortcut('Ctrl+R')
        reloadSettingsAction.setStatusTip('Reload settings from file: ./settings/settings_user.json')
        reloadSettingsAction.triggered.connect(self.main_widget.reloadSettings)

        openFileAction = QtGui.QAction(QtGui.QIcon.fromTheme('document-open'), 'Open netcdf', self)
        openFileAction.setShortcut('Ctrl+O')
        openFileAction.setStatusTip('Open structured grid - netcdf file')
        openFileAction.triggered.connect(self.main_widget.showDialogOpenFile)

        closeFileAction = QtGui.QAction(QtGui.QIcon.fromTheme('window-close'), 'Close netcdf', self)
        closeFileAction.setStatusTip('Close current netcdf file')
        closeFileAction.triggered.connect(self.main_widget.closeFile)

        showCopyrightAction = QtGui.QAction('Copyright', self)
        showCopyrightAction.setStatusTip('Show copyright information')
        showCopyrightAction.triggered.connect(self.showCopyright)

        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&Menu')
        helpMenu = menubar.addMenu('&Help')
        fileMenu.addAction(openFileAction)
        fileMenu.addAction(closeFileAction)
        fileMenu.addAction(reloadSettingsAction)
        fileMenu.addAction(exitAction)
        helpMenu.addAction(showCopyrightAction)
        


    def showCopyright(self):
        info = str(
            "<b>NCINFO, version {0}</b><br>".format(__version__) +
            "This computer program is part of MOSSCO.<br>" +
            "@copyright Copyright (C) 2015 Bundesanstalt fuer Wasserbau<br>" +
            "@author Nikolai Chernikov, BAW<br>" +
            "@contact nikolai.chernikov.ru@gmail.com<br>" +
            "<br>" +
            "<br>" +
            "MOSSCO is free software: you can redistribute it and/or modify it under the " +
            "terms of the GNU General Public License v3+.  MOSSCO is distributed in the " +
            "hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file " +
            "LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.")
        #QtGui.QMessageBox.information(self, 'Copyright', info, QtGui.QMessageBox.Ok)
        QtGui.QMessageBox.about(self, 'Copyright', info)
    

    def closeEvent(self, event):
        event.accept()
        '''
        reply = QtGui.QMessageBox.question(self, 'Message', "Are you sure to quit?", QtGui.QMessageBox.Yes, QtGui.QMessageBox.No)

        if reply == QtGui.QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()
        '''




def main():


    app = QtGui.QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())


if __name__ == '__main__':
    main()