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

from PyQt4.QtCore import Qt, QSettings, QString, QVariant, pyqtSignature
from PyQt4.QtGui import QWidget, QFont, QToolTip, QMenu, QMessageBox, QFileDialog, QApplication
import json

import traceback
import os
import sys
import numpy as np
from pylab import squeeze
import re
from datetime import datetime
from netCDF4 import Dataset
import netCDF4
import matplotlib.cm as cm
import getpass
import time
import threading




# use this if you want to include modules from a subfolder
import inspect
relative_paths = [
    '..',
    '../colorbar_dialog',
    '../flux_dialog',
    '../section_dialog',
    '../project_dlg',
    '../main_mpl_widget/interactive_colorbar',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

import ui_dataselect_dlg
import netcdf_obj as nck
from redirect_stderr_stdout import XStream
from colorbar_dlg import cbarRange_dlg
from plot_transect_dlg import My_dlg as transect_dlg
from plot_flux_dlg import My_dlg as flux_dlg
from interactive_colorbar import DraggableColorbar
from project_dlg import prj_dlg

MAC = "qt_mac_set_native_menubar" in dir()


class selectdata_dlg(QWidget, ui_dataselect_dlg.Ui_Dialog):

    def __init__(self, parent=None, path=None):

        super(selectdata_dlg, self).__init__()
        self._runntimePath = path
        self.read_settings()


        QToolTip.setFont(QFont('SansSerif', 10))
        self.setWindowTitle('netcdf viewer')
        #load flags
        #print 'initing!'
        self.__fileLoaded = False
        self.__dataSelected = False
        self._loadingFile = True
        self._loadingVariable = True
        self.__tIsPresent = False
        self.__xIsPresent = False
        self.__yIsPresent = False
        self.__zIsPresent = False

        self.__t = None
        self.__z = None
        self.__x = None
        self.__y = None
        self._plotType = None
        self.plotType_for_timeseries = None

        self.setupUi(self)

        self.pushButton_cbarRange = self.pltWidget.cbar_button  # pointer to colorbar button
        self.pushButton_cbarRange.clicked.connect(self.on_pushButton_cbarRange_clicked)  #connect to action


        self.checkBox_createTemp.setEnabled(False)  # not functional anylonger

        self.__allWidgedList = [self.comboBox_dataType, self.lineEdit_t,
            self.lineEdit_x, self.horizontalSlider_t, self.lineEdit_tSpinBoxSingleStep,
            self.lineEdit_y, self.lineEdit_z, self.checkBox_tAll, self.checkBox_xAll,
            self.checkBox_yAll, self.checkBox_zAll, self.button_Plot2D, self.spinBox_Timestep, self.spinBox_GridZ,
            self.pushButton_cbarRange, self.pltWidget.grid_cb, self.pltWidget.mpl_toolbar, self.pushButton_plotSpecial,
            self.pushButton_playTime, self.lineEdit_fpsTime]

        self.__tWidgedList = [self.lineEdit_t, self.checkBox_tAll, self.horizontalSlider_t, self.lineEdit_tSpinBoxSingleStep,
                              self.pushButton_playTime, self.lineEdit_fpsTime]
        self.__xWidgedList = [self.lineEdit_x, self.checkBox_xAll]
        self.__yWidgedList = [self.lineEdit_y, self.checkBox_yAll]
        self.__zWidgedList = [self.lineEdit_z, self.checkBox_zAll]


        settings = QSettings()
        self.lastFile = unicode(settings.value("LastFile").toString())
        self.recentFiles = settings.value("RecentFiles").toStringList()[0:6]
        self.filename = None


        self.transect_dlg = None
        self.flux_dlg = None


        self.init_tooltips()

        self.create_context_menu_pushButton_plotSpecial()
        self.update_comboBox_file()
        self.updateUi()
        #self.redirect_output_into_QTextBrowser()

    def init_tooltips(self):
        self.comboBox_file.setToolTip('Select netcdf file')
        self.comboBox_dataType.setToolTip('Select variable from current netcdf file')
        self.checkBox_createTemp.setToolTip('If checked, the script will not open the file directly, but first copy the netcdf to temporary and open this temp copy')
        
        self.lineEdit_t.setToolTip('Select index in t-dimension. Accepts single value (i.e. 1, 23, 100) or ranges (i.e. 0:100, 25:30) <br>Note:<br> - indexing starts from 0 <br> - ranges do not include last value (i.e. 0:2 yields 0, 1 and not 0, 1, 2)')
        self.lineEdit_z.setToolTip('Select index in z-dimension. Accepts single value (i.e. 1, 23, 100) or ranges (i.e. 0:100, 25:30) <br>Note:<br> - indexing starts from 0 <br> - ranges do not include last value (i.e. 0:2 yields 0, 1 and not 0, 1, 2)')
        self.lineEdit_y.setToolTip('Select index in y-dimension. Accepts single value (i.e. 1, 23, 100) or ranges (i.e. 0:100, 25:30) <br>Note:<br> - indexing starts from 0 <br> - ranges do not include last value (i.e. 0:2 yields 0, 1 and not 0, 1, 2)')
        self.lineEdit_x.setToolTip('Select index in x-dimension. Accepts single value (i.e. 1, 23, 100) or ranges (i.e. 0:100, 25:30) <br>Note:<br> - indexing starts from 0 <br> - ranges do not include last value (i.e. 0:2 yields 0, 1 and not 0, 1, 2)')
        
        self.checkBox_tAll.setToolTip('Select all indexes in t-dimension')
        self.checkBox_zAll.setToolTip('Select all indexes in z-dimension')
        self.checkBox_yAll.setToolTip('Select all indexes in y-dimension')
        self.checkBox_xAll.setToolTip('Select all indexes in x-dimension')

        self.spinBox_Timestep.setToolTip('Cycle through timesteps. This will automatically update the plot')
        self.lineEdit_tSpinBoxSingleStep.setToolTip('This field controls the single step of the cycle-through-timesteps-spinbox <br> Accepts integers (try changing and clicking the spinbox above). Default 1')
        self.lineEdit_tSpinBoxSingleStep.setText(QString(u'1'))
        self.horizontalSlider_t.setToolTip('Slider to move through timesteps. Automatically updates the plot with altering')
        self.spinBox_GridZ.setToolTip('Cycle through z-layers. This will automatically update the plot')

        self.pushButton_plotSpecial.setToolTip('special options to be plotted')
        self.button_Save.setToolTip('Save current Tab (Matrix or Plt). <br>Matrix can be saved to XLS or CSV. <br>Plot can be saved to various graphical formats. <br>Automatically generates name based on current selection. It REQUIRES the data to be selected.')
        self.button_Plot2D.setToolTip('Plot current selection! This will also update the Matrix Tab')
        self.button_ClearTextBrowser.setToolTip('Clear the Text Browser in the Log Tab')


        self.pushButton_playTime.setToolTip('Play timesteps, select FPS rate to the right')
        self.lineEdit_fpsTime.setToolTip('Frames per Second for Playing Timesteps')
        self.lineEdit_fpsTime.setText(QString('1'))
        self.label_currentTime.setToolTip('Simulation time of current timestep')

    
    def read_settings(self):
        fname = os.path.join(self._runntimePath, 'settings', 'settings_user.json')
        
        if not os.path.isfile(fname):
            fname = os.path.join(self._runntimePath, 'settings', 'settings_default.json')
        print 'chosen fname:', fname
        with open (fname, "r") as settings_file:
            json_str = settings_file.read()
            settings_file.close()
        self._USERSETTINGS = json.loads(json_str)
        return fname

    def get_usersettings(self):
        return self._USERSETTINGS

    def get_current_nc_fname(self):
        return str(self.comboBox_file.currentText())

    def get_current_var_name(self):
        return str(self.comboBox_dataType.currentText())

    def get_zIsPresent(self):
        return self.__zIsPresent


    def create_context_menu_pushButton_plotSpecial(self):
        # create context menu
        popMenu = QMenu()
        popMenu.addAction('Transection line', self.Action1)
        popMenu.addSeparator()
        popMenu.addAction('Mass fluxes through i,j-section', self.Action2)
        popMenu.addSeparator()
        popMenu.addAction('Create projection', self.Action3)
        self.pushButton_plotSpecial.setMenu(popMenu)

    def Action1(self):
        if self.plotType_for_timeseries == '2DXY':
            try:
                # try to select REAL lat and Lon for a given plot. If no, we cannot open plot.
                self.pltWidget.get_real_xy(0, 0, self.detect_variable_dimensions(log=True))
                open_dlg = True
            except:
                QMessageBox.question(self, 'Error', "lat/lon dimensions not found for this variable. Add them in /settings/settings_user.json and restart the program", QMessageBox.Ok)
                open_dlg = False

            if open_dlg:
                self.transect_dlg = transect_dlg(self)
                self.transect_dlg.show()
        else:
            QMessageBox.question(self, 'Error', "Please select 2D data with X/Y coordinates. Then plot it. Then reselect this tool", QMessageBox.Ok)



    def Action2(self):
        if self.plotType_for_timeseries == '2DXY':
            self.flux_dlg = flux_dlg(self)
            self.flux_dlg.show()

        else:
            QMessageBox.question(self, 'Error', "Please select 2D data with X/Y coordinates. Then plot it. Then reselect this tool", QMessageBox.Ok)


    def Action3(self):
        if self.plotType_for_timeseries == '2DXY':
            self.project_dlg = prj_dlg(self)
            self.project_dlg.show()

        else:
            QMessageBox.question(self, 'Error', "Please select 2D data with X/Y coordinates. Then plot it. Then reselect this tool", QMessageBox.Ok)

    def redirect_output_into_QTextBrowser(self):
        # Install the custom output stream
        #sys.stderr = EmittingStream(textWritten=self.errorOutputWritten)
        # create connections
        #XStream.stdout().messageWritten.connect( self.textBrowser.insertPlainText )
        XStream.stderr().messageWritten.connect(self.textBrowser.append )

    '''
    def errorOutputWritten(self, text):
        """Append text to the QTextEdit."""
        # Maybe QTextEdit.append() works as well, but this is how I do it:
        cursor = self.textBrowser.textCursor()
        cursor.movePosition(QTextCursor.End)
        cursor.insertText(text)
        self.textBrowser.setTextCursor(cursor)
        self.textBrowser.ensureCursorVisible()
    '''


    def showDialogOpenFile(self):
        self.closeFile()
        if self.lastFile :
            openpath = os.path.dirname(self.lastFile)
        else:
            username = getpass.getuser()

            if username == 'ak2mnase':
                openpath = '//Widar/home/ak2mnase/mossco/02_MOSSCO/mossco-setups/'
            elif username == 'ak2stud':
                openpath = '//Widar/home/ak2stud/Nick/'
            else:
                openpath = os.path.dirname(sys.argv[0])  # directory of current script

        fname = unicode(QFileDialog.getOpenFileName(self, 'Open data file', openpath, filter='NetCDF files (*.nc)'))
        if fname:
            self.loadFile(fname)


    def addRecentFile(self, fname):
        if fname is None:
            return
        if not self.recentFiles.contains(fname):
            self.recentFiles.prepend(QString(fname))
            if self.recentFiles.count() > 6:
                self.recentFiles = self.recentFiles[0:7]

        elif self.recentFiles.indexOf(fname) != 0:
            self.recentFiles.move(self.recentFiles.indexOf(fname), 0)

        settings = QSettings()
        settings.setValue("RecentFiles", self.recentFiles)
        settings.setValue("LastFile", QVariant(QString(fname)))
        self.lastFile = fname


    def loadFile(self, fname=None):
        self._loadingFile = True
        if fname:
            self.__fileLoaded = False
            self.closeFile()

            self.updateUi()
            self.filename = None
            path = os.path.dirname(fname)
            filename = os.path.basename(fname)
            self.comboBox_file.setEditText(fname)
            self.label_fileCreationDate.setText('load the file')
            try:
                self.ncdfObj = nck.netCDFobj(path, filename, create_tmp=self.checkBox_createTemp.isChecked(), log=True)
                if self.ncdfObj.get_loadStatus():
                    self.textBrowser.clear()
                    self.__fileLoaded = True
                    self.label_fileCreationDate.setText("Loaded netCDF file was created: {0}".format(self.get_selected_ncfile_creationtime()))
                    self.filename = fname
                    print "File opened: {0}".format(self.filename)
                    self.addRecentFile(self.filename)

                    #message = "Loaded %s" % os.path.basename(fname)
                    self.update_comboBox_dataType()
                    self.update_comboBox_file()
                self.updateUi()
                self._loadingFile = False
            except Exception as e:
                self.updateUi()
                traceback.print_exc()
                msg = QString("<br>Error:<br>{0}<br>{1}<br>{2}<br>".format(type(e), e.args, e) )
                self.textBrowser.append(msg)
                print type(e), e.args, e

                if e.args == (u'NetCDF: Unknown file format',):
                    self.textBrowser.append(QString('Summary of the netCDF4 configuration <br> ----------------------------'))
                    self.textBrowser.append(QString('netcdf4-python version: %s' % netCDF4.__version__))
                    self.textBrowser.append(QString('HDF5 lib version: %s' % netCDF4.__hdf5libversion__))
                    self.textBrowser.append(QString('netcdf lib version: %s' % netCDF4.__netcdf4libversion__))



    def detect_variable_dimensions(self, log=False):
        '''
        functions checks if variable has real dimensions that are also
        specified as variables (in addtition it will check the names of the variables
        added in lists X_DIM_LIST, Y_DIM_LIST)

        Lets assume we have following dimensions in netcdf file (100x50 grid):
            - lat(100)
            - lon(50)
            - sigma_layer(10)
        and following variable:
            - lat(lat)
            - lon(lon)
            - data(sigma_layer, lat, lon)

        Then the output of this script for variable $data$ will be list [None, lat, lon]
        '''

        X_DIM_LIST = self.get_usersettings()['lon_dims']
        Y_DIM_LIST = self.get_usersettings()['lat_dims']
        nc = self.get_selected_ncfile_instance()
        dim_names = None
        if self.__dataSelected:
            varname = self.get_current_var_name()  # string! not Qstring

            dim_names = list(nc.variables[varname].dimensions)
            if dim_names:  # if variable has dimensions
                for i, dn in enumerate(dim_names):
                    if dn not in nc.variables.keys():  # if dimension name is in variables
                        dim_names[i] = None


            # if X was not found, try one from user defined list
            if dim_names[-1] is None:
                for user_x_dn in X_DIM_LIST:
                    if user_x_dn in nc.variables.keys():
                        dim_names[-1] = user_x_dn
            # if Y was not found, try one from user defined list
            if dim_names[-2] is None:
                for user_y_dn in Y_DIM_LIST:
                    if user_y_dn in nc.variables.keys():
                        dim_names[-2] = user_y_dn

        if log: print 'detect_variable_dimensions() >>>', dim_names
        nc.close()
        return dim_names



    def update_comboBox_file(self):
        self.comboBox_file.clear()
        self.comboBox_file.addItem(unicode("...Open  File..."))
        self.comboBox_file.addItem(unicode("...Close File..."))
        self.comboBox_file.addItem(unicode("----------------"))
        current = QString(self.filename) if self.filename is not None else "Select File"
        self.comboBox_file.setEditText(current)
        for fname in self.recentFiles:
            self.comboBox_file.addItem(unicode(fname))


    def update_comboBox_dataType(self):
        self.comboBox_dataType.clear()
        if self.__fileLoaded:
            try:
                nc = self.get_selected_ncfile_instance()
                for var in nc.variables.keys():
                    self.comboBox_dataType.addItem(unicode(var))
                nc.close()
            except:
                try:
                    nc.close()
                except:
                    pass
                raise TypeError("Cannot find variables in NC file. Check if the file is damaged")



    def updateTSelectionWidgets(self):
        self.checkBox_tAll.setCheckState(Qt.Unchecked)
        enabled = self.__tIsPresent
        for widget in self.__tWidgedList:
            widget.setEnabled(enabled)
        self.checkBox_tAll.setText(QString("All (0:{0})".format(self.__t-1))) if self.__t is not None else self.checkBox_tAll.setText(QString("All (?:?)"))

        if not enabled:
            self.lineEdit_t.clear()
            self.spinBox_Timestep.cleanText()
            self.horizontalSlider_t.setSliderPosition(0)
            
        if enabled and self.__t is not None and self.__dataSelected:
            if str(self.lineEdit_t.text()) == '':
                self.lineEdit_t.setText(u"0")



    def updateZSelectionWidgets(self):
        self.checkBox_zAll.setCheckState(Qt.Unchecked)
        enabled = self.__zIsPresent
        self.lineEdit_z.setEnabled(enabled)
        self.checkBox_zAll.setEnabled(enabled)
        self.spinBox_GridZ.setEnabled(enabled)

        if not enabled:
            self.lineEdit_z.clear()
            self.spinBox_GridZ.cleanText()
        
        self.checkBox_zAll.setText(QString("All (0:{0})".format(self.__z-1))) if self.__z is not None else self.checkBox_zAll.setText(QString("All (?:?)"))
        
        if enabled and self.__z is not None and self.__dataSelected:
            if str(self.lineEdit_z.text()) == '':
                self.lineEdit_z.setText("0")


    def updateXSelectionWidgets(self):
        self.checkBox_xAll.setCheckState(Qt.Unchecked)
        enabled = self.__xIsPresent
        self.lineEdit_x.setEnabled(enabled)
        if not enabled:
            self.lineEdit_x.clear()
        self.checkBox_xAll.setEnabled(enabled)
        self.checkBox_xAll.setText(QString("All (0:{0})".format(self.__x-1))) if self.__x is not None else self.checkBox_xAll.setText(QString("All (?:?)"))
        
        if enabled and self.__x is not None and self.__dataSelected:
            if str(self.lineEdit_x.text()) == '':
                self.lineEdit_x.setText("0")


    def updateYSelectionWidgets(self):
        self.checkBox_yAll.setCheckState(Qt.Unchecked)
        #if self.lineEdit_y.text() is None:
        #    self.lineEdit_y.clear()
        enabled = self.__yIsPresent
        self.lineEdit_y.setEnabled(enabled)
        if not enabled:
            self.lineEdit_y.clear()
        self.checkBox_yAll.setEnabled(enabled)
        self.checkBox_yAll.setText(QString("All (0:{0})".format(self.__y-1))) if self.__y is not None else self.checkBox_yAll.setText(QString("All (?:?)"))
        if enabled and self.__y is not None and self.__dataSelected:
            if str(self.lineEdit_y.text()) == '':
                self.lineEdit_y.setText("0")


    def updateTextBrowser(self):
        if self.__fileLoaded and not self.__dataSelected:  #file is loaded
            nc = self.get_selected_ncfile_instance()
            if not self.checkBox_createTemp.isChecked():
                load_msg = QString('<font color=blue> File loaded: <br> {0}<br> {1} <br></font>'.format(self.filename, '-'*50))
            else:
                load_msg = QString('<font color=blue> Working with temp copy. Copied data file to a temp file:<br>  data: {0} <br>  temp: {1} <br> {2} <br></font>'.format(
                                    self.get_current_nc_fname(), self.ncdfObj.get_fname()[1], '-'*50))
            msg = nc.__str__()
            try:
                msg1, msg2 = msg.split('variables(dimensions)')
                msg = QString(msg1+'\n\n'+'variables(dimensions)'+msg2)
            except:
                msg = QString(msg)
            self.textBrowser.append(load_msg)
            self.textBrowser.append(msg)
            nc.close()

        if self.__dataSelected:  # if variable is loaded
            nc = self.get_selected_ncfile_instance()
            varname = self.get_current_var_name()  # string! not Qstring
            load_msg = QString('<font color=blue> <br> {1} <br> Variable info: <br> {0}<br> {1} <br></font>'.format(varname, '-'*50))
            msg = QString(nc.variables[varname].__str__())
            msg2 = QString('min, max: {0}, {1}'.format(self.selected_data['vmin'], self.selected_data['vmax']))
            self.textBrowser.append(load_msg)
            self.textBrowser.append(msg)
            self.textBrowser.append(msg2)
            nc.close()

        if not self.__fileLoaded:
            self.textBrowser.clear()
        self.tabWidget.setCurrentWidget(self.tab)


    def updateUi(self, DoNotResetTableView=False):
        if not self.__fileLoaded:
            self.comboBox_file.setCurrentIndex(0)
            self.comboBox_dataType.clear()

        if self._loadingVariable or self._loadingFile:  #if we are currently loading either file or variable
            for widget in self.__allWidgedList:
                widget.setEnabled(False)

            self.comboBox_dataType.setEnabled(self.__fileLoaded)
            self.button_Plot2D.setEnabled(self.__dataSelected)
            self.pushButton_plotSpecial.setEnabled(self.__dataSelected)
            self.updateTextBrowser()

            self.updateTSelectionWidgets()
            self.updateZSelectionWidgets()
            self.updateXSelectionWidgets()
            self.updateYSelectionWidgets()

        if self.__dataSelected:
            if self.__tIsPresent:
                #self.spinBox_Timestep.setEnabled(self.__tIsPresent)
                self.spinBox_Timestep.setMinimum(0)
                self.spinBox_Timestep.setMaximum(self.__t-1)
                self.horizontalSlider_t.setMinimum(0)
                self.horizontalSlider_t.setMaximum(self.__t-1)

            if self.__zIsPresent:
                #self.spinBox_GridZ.setEnabled(self.__zIsPresent)
                self.spinBox_GridZ.setMinimum(0)
                self.spinBox_GridZ.setMaximum(self.__z-1)

        # reset table in Tab Widget
        if not DoNotResetTableView:
            self.tableView.setArrayData(np.arange(0))


    def reloadSettings(self):
        fname = self.read_settings()
        info = str('Setting were successfully read from:<br>' + fname)
        QMessageBox.information(self, 'Message', info , QMessageBox.Ok)



    def closeFile(self):
        try:
            #self.ncdfObj.close()
            #del(self.ncdfObj)
            self.label_fileCreationDate.setText('load the file')
            #print "File closed: {0}\n".format(self.filename)
            print "File closed: {0}\n".format(self.get_current_nc_fname())
            self.filename = None
            self.__fileLoaded = False
            self.__dataSelected = False
            self.__tIsPresent = False
            self.__xIsPresent = False
            self.__yIsPresent = False
            self.__zIsPresent = False
            self._loadingFile = True
            self.updateUi()
            self.pltWidget.canvas.clear()
        except:
            pass

    def checkLineEditText(self, lineEdit, maxValue, update_lineedit=True):
        text = unicode(lineEdit.text())
        if text:
            try:  # if we have only one timestep
                selected_t = int(text)
                if selected_t < 0:
                    selected_t = 0
                if selected_t > maxValue:
                    selected_t = maxValue
                if update_lineedit: lineEdit.clear()
                if update_lineedit: lineEdit.setText(QString("{0}".format(selected_t)))
                return (selected_t,)

            except ValueError:  # if we cannot convert to integer
                try:  # if we have a region ( int:int )
                    first_t, last_t = text.split(":")[0:2]
                    f_t = int(first_t)
                    l_t = int(last_t)
                    if f_t < 0: f_t = 0
                    if l_t > maxValue: l_t = maxValue
                    if f_t == l_t:  # we dont want to have regions like (3:3)
                        if update_lineedit: lineEdit.clear()
                        if update_lineedit: lineEdit.setText(QString("{0}".format(f_t)))
                        return (f_t,)
                except ValueError:  # wrong user input!
                    f_t = 0
                    l_t = maxValue

                if update_lineedit: lineEdit.clear()
                if update_lineedit: lineEdit.setText(QString("{0}:{1}".format(f_t, l_t)))
                return (f_t, l_t)

    @pyqtSignature("QString")
    def on_comboBox_file_activated(self, text):
        text = unicode(text)
        if text != self.filename and text != "...Open  File...":
            self.closeFile()
            self.updateUi()
            self.pltWidget.fig.clf()
        if text == "...Open  File...":
            self.showDialogOpenFile()
            self.pltWidget.fig.clf()
        elif text != "...Open  File..." and text != "...Close File..." and text != "----------------" and text != self.filename:
            self.loadFile(text)

    def load_vmin_vmax(self, var):
        ''' var - netcdf4 instance variable
        '''
        if any(a in self.get_usersettings()['valid_range_attribute_names'] for a in var.ncattrs()):
            for a in self.get_usersettings()['valid_range_attribute_names']:
                if a in var.ncattrs(): break
            attr = var.getncattr(a)
            try:
                vmin, vmax = attr[0], attr[1]
                print 'min, max values detected in NC attribute'
            except:
                vmin, vmax = self.get_selected_data_min_max(current_selection=False)  # find min/max in whole array
        else:
            vmin, vmax = self.get_selected_data_min_max(current_selection=False)  # find min/max in whole array

        return vmin, vmax

    def get_time1D_array(self):
        t_found = False
        nc = self.get_selected_ncfile_instance()
        for t_dim in self.get_usersettings()['t_dims']:
            if t_dim in nc.variables.keys():
                t_found = True
                t = nc.variables[t_dim][:]
                nc.close()
                return t
                
        if not t_found:
            nc.close()
            msg = 'Time variable not found'
            QMessageBox.question(self, 'Error', msg, QMessageBox.Ok)
            return None

    @pyqtSignature("QString")
    def on_comboBox_dataType_activated(self, text):
        self.__dataSelected = False
        self.__tIsPresent = False
        self.__xIsPresent = False
        self.__yIsPresent = False
        self.__zIsPresent = False
        var = unicode(text)
        nc = self.get_selected_ncfile_instance()
        loadedVariable = nc.variables[var]
        #currentDataShape = loadedVariable.shape
        if loadedVariable:

            
            #try: del(self.cb)  # also try to delete colorbar no disable on_mouse_release event
            #except: pass


            self._loadingVariable = True
            self.__dataSelected = True


            self.selected_data = dict()  # selected data

            self.selected_data['vmin'], self.selected_data['vmax'] = self.load_vmin_vmax(loadedVariable)


            dims_n = loadedVariable.dimensions
            dims_v = loadedVariable.shape

            self.__t = None
            self.__z = None
            self.__x = None
            self.__y = None

            for dim_n, dim_v in zip(dims_n, dims_v):
                if dim_n in self.get_usersettings()['t_dims']:
                    self.__tIsPresent = True
                    self.__t = dim_v
                elif dim_n in self.get_usersettings()['z_dims']:
                    self.__zIsPresent = True
                    self.__z = dim_v
                elif dim_n in self.get_usersettings()['y_dims']:
                    self.__yIsPresent = True
                    self.__y = dim_v
                elif dim_n in self.get_usersettings()['x_dims']:
                    self.__xIsPresent = True
                    self.__x = dim_v
                else:
                    msg = "Dimension <i>{0}</i> of length <i>{1}</i> not recognized in varibale <i>{2}</i>.<br>Add it in file <b>/settings/settings_user.json</b> and reload settings:<br> Menu -> Reload Settings".format(
                        dim_n, dim_v, var)
                    QMessageBox.question(self, 'Error', msg, QMessageBox.Ok)

            for MAX, checkBox, lineedit, found in zip([self.__t, self.__z, self.__x, self.__y],
                        [self.checkBox_tAll, self.checkBox_zAll, self.checkBox_xAll, self.checkBox_yAll],
                        [self.lineEdit_t, self.lineEdit_z, self.lineEdit_y, self.lineEdit_x],
                        [self.__tIsPresent, self.__zIsPresent, self.__xIsPresent, self.__yIsPresent]):
                if found:
                    checkBox.setText(QString("All (0:{0})".format(MAX)))
            #print [self.__fileLoaded, self.__dataSelected, self.__tIsPresent, self.__xIsPresent, self.__yIsPresent, self.__zIsPresent]

            self.updateUi()
            self._loadingVariable = False
        nc.close()

    @pyqtSignature("int")
    def on_tabWidget_currentChanged(self, state_int):
        if state_int == 0:  # if log tab
            self.button_Save.setEnabled(False)
            self.button_ClearTextBrowser.setEnabled(True)

        if state_int == 1:  # if matrix tab
            self.button_Save.setEnabled(True)
            self.button_ClearTextBrowser.setEnabled(False)

        if state_int == 2:  # if plot tab
            self.button_Save.setEnabled(True)
            self.button_ClearTextBrowser.setEnabled(False)



    @pyqtSignature("int")
    def on_checkBox_tAll_stateChanged(self, state_int):
        widgets_to_disable = [self.lineEdit_t, self.horizontalSlider_t, self.lineEdit_tSpinBoxSingleStep,
                              self.pushButton_playTime, self.lineEdit_fpsTime]
        for w in widgets_to_disable:
            w.setEnabled(True)
        if state_int == 2:
            for w in widgets_to_disable:
                w.setEnabled(False)
            self.lineEdit_t.setText("0:{0}".format(self.__t))

    @pyqtSignature("int")
    def on_checkBox_zAll_stateChanged(self, state_int):
        self.lineEdit_z.setEnabled(True)
        self.spinBox_GridZ.setEnabled(True)
        if state_int == 2:
            self.lineEdit_z.setEnabled(False)
            self.spinBox_GridZ.setEnabled(False)
            self.lineEdit_z.setText("0:{0}".format(self.__z))

    @pyqtSignature("int")
    def on_checkBox_xAll_stateChanged(self, state_int):
        self.lineEdit_x.setEnabled(True)
        if state_int == 2:
            self.lineEdit_x.setEnabled(False)
            self.lineEdit_x.setText("0:{0}".format(self.__x))

    @pyqtSignature("int")
    def on_checkBox_yAll_stateChanged(self, state_int):
        self.lineEdit_y.setEnabled(True)
        if state_int == 2:
            self.lineEdit_y.setEnabled(False)
            self.lineEdit_y.setText("0:{0}".format(self.__y))

    def set_selected_timestep_label(self, t=''):
        start = time.clock()
        selected_t = self.checkLineEditText(self.lineEdit_t, self.__t, update_lineedit=False)
        nc = self.get_selected_ncfile_instance()
        real_time = False
        # use real time dims...
        try:
            pat_start_time = re.compile('.*?(\d\d\d\d.\d\d.\d\d.\d\d.\d\d.\d\d).*?', re.DOTALL)
            t_start = re.match(pat_start_time, nc.variables['time'].units).group(1)
            t_start = re.sub('[:;/,\.\-]', '/', t_start)
            t_start = datetime.datetime.strptime(t_start, '%Y/%m/%d %H/%M/%S')
            real_time = True
        except Exception as e:
                print e
                msg = "Time units not recognized"

        if real_time:
            if len(selected_t) == 2:  #region
                t1 = t_start + datetime.timedelta(seconds=nc.variables['time'][selected_t[0]])
                t2 = t_start + datetime.timedelta(seconds=nc.variables['time'][selected_t[1]-1])
                msg = '{0}:{1}'.format(datetime.datetime.strftime(t1, "%Y/%m/%d %H:%M:%S"), datetime.datetime.strftime(t2, "%Y/%m/%d %H:%M:%S") )
            elif len(selected_t) == 1:  #single val
                t1 = t_start + datetime.timedelta(seconds=nc.variables['time'][selected_t[0]])
                msg = '{0}'.format(datetime.datetime.strftime(t1, "%Y/%m/%d %H:%M:%S"))
        nc.close()

        self.label_currentTime.setText(QString(msg))
        end = time.clock()
        print '+++++++ t={1}, FPS = {0} (from set_time_label) +++++++'.format(1./(end - start), t)

    @pyqtSignature("QString")
    def on_lineEdit_t_textChanged(self, text):
        self.set_selected_timestep_label(t=str(text))

    @pyqtSignature("")
    def on_lineEdit_z_editingFinished(self):
        self.checkLineEditText(self.lineEdit_z, self.__z)

    @pyqtSignature("")
    def on_lineEdit_x_editingFinished(self):
        self.checkLineEditText(self.lineEdit_x, self.__x)

    @pyqtSignature("")
    def on_lineEdit_y_editingFinished(self):
        self.checkLineEditText(self.lineEdit_y, self.__y)


    @pyqtSignature("")
    def on_lineEdit_tSpinBoxSingleStep_editingFinished(self):
        self.spinBox_Timestep.setSingleStep(self.get_spinBox_t_increment())


    def toggle_timestep_player(self, fps=1):

        t_max = self.__t
        selected_t = self.checkLineEditText(self.lineEdit_t, self.__t, update_lineedit=False)

        if len(selected_t) == 1:
            global i
            i = int(selected_t[0])-1
            global step
            step = self.get_spinBox_t_increment()

            def play ():
                global i
                global step
                print 'step =', i
                i += step
                self._timePlayer = threading.Timer(float(1/fps), play)
                
                if i >= t_max:
                    #print 'maximum reached, going to 0', i
                    i = 0
                
                self._timePlayer.start ()
                self.spinBox_Timestep.setValue(i)
                
            play()

    def get_spinBox_t_increment(self):
        return int(str(self.lineEdit_tSpinBoxSingleStep.text()))

    def get_playTimesteps_FPS(self):
        return float(str(self.lineEdit_fpsTime.text() ) )
    
    def start_timestepPlayer(self):
        self.__disabled_widgets_by_play_button = [
                self.comboBox_file, self.checkBox_createTemp,
                self.comboBox_dataType,
                self.lineEdit_x,    self.lineEdit_y,    self.lineEdit_z,    self.lineEdit_t,
                self.checkBox_xAll, self.checkBox_yAll, self.checkBox_zAll, self.checkBox_tAll,
                
                self.horizontalSlider_t, self.lineEdit_tSpinBoxSingleStep, self.spinBox_Timestep,
                self.lineEdit_fpsTime,
                
                self.spinBox_GridZ,
                self.button_Plot2D, self.pushButton_plotSpecial, self.button_Save, self.button_ClearTextBrowser
                ]

        # remembering settings before and disabling widgets....
        self._at_clicked_playTime_EnabledSettings = list()
        for w in self.__disabled_widgets_by_play_button:
            self._at_clicked_playTime_EnabledSettings.append(w.isEnabled())
            w.setEnabled(False)
        
        

        self.pushButton_playTime.setFlat(True)
        self.pushButton_playTime.setText(QString('Stop'))
        
        # starting player...
        fps = self.get_playTimesteps_FPS()
        self.toggle_timestep_player(fps=fps)

    def stop_timestepPlayer(self):
            # stopping player
            self._timePlayer.cancel()
            # loading settings before....
            for w, enabled in zip(self.__disabled_widgets_by_play_button, self._at_clicked_playTime_EnabledSettings):
                w.setEnabled(enabled)
            
            self.pushButton_playTime.setFlat(False)
            self.pushButton_playTime.setText(QString('Play'))
    

    @pyqtSignature("")
    def on_pushButton_playTime_clicked(self):
        if self.pushButton_playTime.isFlat():
            #self.pushButton_playTime.setStyleSheet('QPushButton {background-color: None}')
            self.stop_timestepPlayer()
        else:
            #self.pushButton_playTime.setStyleSheet('QPushButton {background-color: red}')
            self.start_timestepPlayer()

    @pyqtSignature("")
    def on_button_ClearTextBrowser_clicked(self):
        self.textBrowser.clear()
        self.pltWidget.fig.clear()

    def on_pushButton_cbarRange_clicked(self):
        ''' See interactive
            http://stackoverflow.com/questions/5611805/using-matplotlib-slider-widget-to-change-clim-in-image
        '''
        dlg = cbarRange_dlg(self, cbar=self.cb)
        if dlg.exec_():  # if accepted
            cbar_min = dlg.getMin()
            cbar_max = dlg.getMax()
            #self.im.set_clim([cbar_min, cbar_max])
            self.cb.set_min(cbar_min)
            self.cb.set_max(cbar_max)
            #self.pltWidget.fig.canvas.draw()



    @pyqtSignature("")
    def on_button_Plot2D_clicked(self):
        self.pltWidget.grid_cb.setCheckState(Qt.Unchecked)
        vmin, vmax = self.selected_data['vmin'], self.selected_data['vmax']
        self.plot2D_hardcode(cb_min=vmin, cb_max=vmax)

    @pyqtSignature("")
    def on_button_Save_clicked(self):
        if self.tabWidget.currentWidget() is self.tab_3:  #if plot window is opened
            fname = unicode(QFileDialog.getSaveFileName(self, 'Save As', directory=self._exportCaption,
                        filter='ALL images (*.jpg *.png *.bmp *.pdf);; JPEG (*.jpg);; PNG (*.png);; BMP (*.bmp);; PDF (*.pdf)'))
            if fname:
                self.pltWidget.fig.savefig(fname)


        elif self.tabWidget.currentWidget() is self.tab_2:  # if matrix tab is opened
            def writeToCSV(qtw, filename, separator=';'):
                f = open(filename, 'w')
                cLinea = ""
                for fila in xrange(qtw.get_currentItemModel().rowCount()):
                    for columna in xrange(qtw.get_currentItemModel().columnCount()):
                        index = qtw.get_currentItemModel().index(fila, columna)
                        cLinea += str(qtw.get_currentItemModel().data(index, role=Qt.DisplayRole).toString()) + separator
                    cLinea = cLinea[:-1]+"\n"
                    f.write(cLinea)
                    cLinea = ""
                f.close()

            filename, filter_dtype = QFileDialog.getSaveFileNameAndFilter(self, 'Save matrix as', directory=self._exportCaption,
                        filter="csv (*.csv);;xls (*.xls)")
            filename, filter_dtype = unicode(filename), unicode(filter_dtype)
            if filename:
                if filter_dtype == 'csv (*.csv)': separator = ','
                elif filter_dtype == 'xls (*.xls)': separator = ';'

                writeToCSV(self.tableView, filename, separator=separator)


    @pyqtSignature("int")
    def on_horizontalSlider_t_valueChanged(self, new_value):
        self.spinBox_Timestep.setValue(new_value)



    @pyqtSignature("int")
    def on_spinBox_Timestep_valueChanged(self, new_value):
        #print 'timestep spinbox value changed'
        self.lineEdit_t.setText(str(new_value))
        self.horizontalSlider_t.setSliderPosition(new_value)
        if not self._doNotPlotSecondTime:
            #self.plot2D_hardcode()
            self.imshow_setData(msg=str(new_value))

    @pyqtSignature("int")
    def on_spinBox_GridZ_valueChanged(self, new_value):
        #print 'gridz spinbox value changed'
        self.lineEdit_z.setText(str(new_value))
        if not self._doNotPlotSecondTime:
            #self.plot2D_hardcode()
            self.imshow_setData()

    def closeEvent(self, event):
        reply = QMessageBox.question(self, 'Message', "Are you sure to quit?", QMessageBox.Yes, QMessageBox.No)

        if reply == QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()

    def get_values_to_select_data(self):
        selection_regions = [False]*4
        selection_points = [False]*4
        dims_present = [False]*4

        #t1, t2, z1, z2, y1, y2, x1, x2 = [None]*8
        v = [None]*8
        for flag, lineEdit, checkBoxAll, index in zip([self.__tIsPresent, self.__zIsPresent, self.__yIsPresent, self.__xIsPresent],
                                                  [self.lineEdit_t, self.lineEdit_z, self.lineEdit_y, self.lineEdit_x],
                                                  [self.checkBox_tAll, self.checkBox_zAll, self.checkBox_yAll, self.checkBox_xAll],
                                                  [0, 2, 4, 6]):
            if flag:
                dims_present[index/2] = True
                s = str(lineEdit.text())
                if checkBoxAll.isChecked():
                    v[index], v[index+1] = [int(t) for t in s.split(':')]
                    v[index+1] += 1
                    selection_regions[index/2] = True
                else:
                    #
                    # HARCODE assume that we only have ONE value not '12:43:234' !!! implement validation!
                    #
                    try:
                        v[index], v[index+1] = [int(t) for t in s.split(':')]
                        v[index+1] += 1
                        selection_regions[index/2] = True
                    except:
                        try:
                            v[index] = int(s)
                            v[index+1] = v[index]+1
                            selection_points[index/2] = True
                        except:
                            QMessageBox.question(self, 'Error', "Select dimensions correctly", QMessageBox.Ok)

        t1, t2, z1, z2, y1, y2, x1, x2 = v
        ndims = sum(dims_present)
        return t1, t2, z1, z2, y1, y2, x1, x2, ndims, selection_regions, selection_points, dims_present


    def get_selected_data_min_max(self, current_selection=True):
        ''' if current_selection=False > return min/max of whole array
            if current_selection=True  > return min/max of selected 1d/2d array
        '''
        if not self.__dataSelected: return (None, None)
        nc = self.get_selected_ncfile_instance()
        var = nc.variables[self.get_current_var_name()]
        if current_selection:
            z, _ = self.detectPlotType_andReturnData(var)
            vmin = z.min()
            vmax = z.max()
        else:
            vmin = var[:].min()
            vmax = var[:].max()
        nc.close()
        return (vmin, vmax)

    def get_selected_ncfile_instance(self):
        return  Dataset(self.get_current_nc_fname(), mode='r')
    
    def get_selected_ncfile_creationtime(self, fname=None):
        # retreive file-creation time
        if not fname:
            return time.ctime(os.path.getctime(self.get_current_nc_fname()))
        else:
            return time.ctime(os.path.getctime(fname))

    def get_nTimesteps(self):
        return self.__t

    def get_nZ(self):
        return self.__z

    def get_nY(self):
        return self.__y

    def get_nX(self):
        return self.__x

    def get_tIsPresent(self):
        #print 't is present?:', self.__tIsPresent
        return self.__tIsPresent

    def get_selectedT(self):
        return int(self.lineEdit_t.text())
    
    def get_plotType(self):
        return self._plotType

    def get_plotType_for_timeseries(self):
        return self.plotType_for_timeseries

    def detectPlotType_andReturnData(self, nc_variable, manually=False, detect_plot_type=True):
        var = nc_variable
        if not manually:
            t1, t2, z1, z2, y1, y2, x1, x2, ndims, isSelectedRegion, isSelectedPoint, dimensionIsPresent = self.get_values_to_select_data()
        else:
            t1, t2, z1, z2, y1, y2, x1, x2, ndims, isSelectedRegion, isSelectedPoint, dimensionIsPresent = manually
        #print '-'*25
        #print 't1, t2, z1, z2, y1, y2, x1, x2, ndims, \nisSelectedRegion \nisSelectedPoint \ndimensionIsPresent'
        #print t1, t2, z1, z2, y1, y2, x1, x2, ndims
        #print isSelectedRegion
        #print isSelectedPoint
        #print dimensionIsPresent
        #print '-'*25
        z = np.zeros(0)  # initialization


        if sum(isSelectedRegion) > 2:  # sum([True, False, False, True]) = 2
            QMessageBox.question(self, 'Message', "Selected data is not 2D or 1D. Cannot be displayed", QMessageBox.Ok)

        else:
            #find which dimensions are selected as regions...
            dimsSelectedRegions = []
            for i, isRegion in enumerate(isSelectedRegion):
                if isRegion and dimensionIsPresent[i]:  #find which dimensions are selected as regions...
                    dimsSelectedRegions.append(i)


            # find index of missing dimesion
            missingDimList = []
            presentDimList = [0, 1, 2, 3]
            for i, dim in enumerate(dimensionIsPresent):
                if not dim:
                    missingDimList.append(i)
                    presentDimList.remove(i)


            if detect_plot_type: self._plotType = None
            if detect_plot_type: self.plotType_for_timeseries = None

            if sum(isSelectedRegion) == 2:
                if detect_plot_type: self._plotType = '2D'

                if ndims == 4:
                    z = squeeze(var[t1:t2, z1:z2, y1:y2, x1:x2])
                    if dimsSelectedRegions == [0, 1]:  #if time, z are regions
                        self._exportCaption = '{0}__depth_timeseries__GridX={1}_GridY={2}'.format(self.get_current_var_name(), x1, y1)
                    elif dimsSelectedRegions == [0, 2]:  #if time, y are regions
                        self._plotType = None
                        QMessageBox.question(self, 'Message', "Selected data is TIME-Y 2D. Not yet implemented", QMessageBox.Ok)
                    elif dimsSelectedRegions == [0, 3]:  #if time, x are regions
                        self._plotType = None
                        QMessageBox.question(self, 'Message', "Selected data is TIME-X 2D. Not yet implemented", QMessageBox.Ok)
                    elif dimsSelectedRegions == [1, 2]:  #if z, y are regions
                        self._exportCaption = '{0}__Z-Y__timestep={2}_GridX={1}'.format(self.get_current_var_name(), x1, t1)
                        #QMessageBox.question(self, 'Message', "Selected data is Z-Y 2D. Not yet implemented", QMessageBox.Ok)
                    elif dimsSelectedRegions == [1, 3]:  #if z, x are regions
                        self._exportCaption = '{0}__ZX__timestep={2}_GridY={1}'.format(self.get_current_var_name(), y1, t1)
                        #QMessageBox.question(self, 'Message', "Selected data is Z-X 2D. Not yet implemented", QMessageBox.Ok)
                    elif dimsSelectedRegions == [2, 3]:  # if x,y are regions
                        self._exportCaption = '{0}__GridZ={1}__timestep={2}'.format(self.get_current_var_name(), z1, t1)

                elif ndims == 3:
                    if missingDimList == [0]:  # time is missing
                        z = squeeze(var[z1:z2, y1:y2, x1:x2])
                        if dimsSelectedRegions == [1, 2]:  #if z, y are regions
                            self._exportCaption = '{0}__Z-Y__timestep={2}_GridX={1}'.format(self.get_current_var_name(), x1, t1)
                            #QMessageBox.question(self, 'Message', "Selected data is Z-Y 2D. Not yet implemented", QMessageBox.Ok)
                        elif dimsSelectedRegions == [1, 3]:  #if z, x are regions
                            self._exportCaption = '{0}__ZX__timestep={2}_GridY={1}'.format(self.get_current_var_name(), y1, t1)
                            #QMessageBox.question(self, 'Message', "Selected data is Z-X 2D. Not yet implemented", QMessageBox.Ok)
                        elif dimsSelectedRegions == [2, 3]:  # if x,y are regions
                            self._exportCaption = '{0}__X-Y_GridZ={1}'.format(self.get_current_var_name(), z1)

                    elif missingDimList == [1]:  # z is missing
                        z = squeeze(var[t1:t2, y1:y2, x1:x2])
                        if dimsSelectedRegions == [0, 2]:  #if time, y are regions
                            self._plotType = None
                            QMessageBox.question(self, 'Message', "Selected data is TIME-Y 2D. Not yet implemented", QMessageBox.Ok)
                        elif dimsSelectedRegions == [0, 3]:  #if time, x are regions
                            self._plotType = None
                            QMessageBox.question(self, 'Message', "Selected data is TIME-X 2D. Not yet implemented", QMessageBox.Ok)
                        elif dimsSelectedRegions == [2, 3]:  # if x,y are regions
                            self._exportCaption = '{0}__XY_timestep={1}'.format(self.get_current_var_name(), t1)

                    elif missingDimList == [2]:  # y is missing
                        z = squeeze(var[t1:t2, z1:z2, x1:x2])
                        if dimsSelectedRegions == [0, 1]:  #if time, z are regions
                            self._exportCaption = '{0}__GridZ_timeseries__GridX={1}'.format(self.get_current_var_name(), x1)
                        elif dimsSelectedRegions == [0, 3]:  #if time, x are regions
                            self._plotType = None
                            QMessageBox.question(self, 'Message', "Selected data is TIME-X 2D. Not yet implemented", QMessageBox.Ok)
                        elif dimsSelectedRegions == [1, 3]:  #if z, x are regions
                            self._exportCaption = '{0}__ZX__timestep={2}_GridY={1}'.format(self.get_current_var_name(), y1, t1)
                            #QMessageBox.question(self, 'Message', "Selected data is Z-X 2D. Not yet implemented", QMessageBox.Ok)

                    elif missingDimList == [3]:  # x is missing
                        z = squeeze(var[t1:t2, z1:z2, y1:y2])
                        if dimsSelectedRegions == [0, 1]:  #if time, z are regions
                            self._exportCaption = '{0}__GridZ_timeseries__GridY={1}'.format(self.get_current_var_name(), y1)
                        elif dimsSelectedRegions == [0, 2]:  #if time, y are regions
                            self._plotType = None
                            QMessageBox.question(self, 'Message', "Selected data is TIME-Y 2D. Not yet implemented", QMessageBox.Ok)
                        elif dimsSelectedRegions == [1, 2]:  #if z, y are regions
                            self._exportCaption = '{0}__ZY__timestep={2}_GridX={1}'.format(self.get_current_var_name(), x1, t1)
                            #QMessageBox.question(self, 'Message', "Selected data is Z-Y 2D. Not yet implemented", QMessageBox.Ok)


                elif ndims == 2:
                    if missingDimList == [0, 1]:  # time, z are missing
                        #print y1,y2, x1,x2
                        z = squeeze(var[y1:y2, x1:x2])
                        self._exportCaption = '{0}_YX'.format(self.get_current_var_name())
                    if missingDimList == [0, 2]:  # time, y are missing
                        z = squeeze(var[z1:z2, x1:x2])
                        self._exportCaption = '{0}_ZX'.format(self.get_current_var_name())
                    if missingDimList == [0, 3]:  # time, x are missing
                        z = squeeze(var[z1:z2, y1:y2])
                        self._exportCaption = '{0}_ZY'.format(self.get_current_var_name())
                    if missingDimList == [1, 2]:  # z, y are missing
                        z = squeeze(var[t1:t2, x1:x2])
                        self._exportCaption = '{0}_TX'.format(self.get_current_var_name())
                    if missingDimList == [1, 3]:  # z, x are missing
                        z = squeeze(var[t1:t2, y1:y2])
                        self._exportCaption = '{0}_TY'.format(self.get_current_var_name())
                    if missingDimList == [2, 3]:  # y, x are missing
                        z = squeeze(var[t1:t2, z1:z2])
                        self._exportCaption = '{0}_TZ'.format(self.get_current_var_name())

            elif sum(isSelectedRegion) == 1:
                if detect_plot_type: self._plotType = '1D'
                if ndims == 4:
                    print '-'*25
                    print 'Slicing data for timeseries plot:'
                    print '\tt1:t2 = {0}:{1}'.format(t1, t2), 'z1:z2 = {0}:{1}'.format(z1, z2), 'y1:y2 = {0}:{1}'.format(y1, y2), 'x1:x2 = {0}:{1}'.format(x1, x2)
                    print '\tvariable has shape of :', var.shape
                    print '-'*25

                    z = squeeze(var[t1:t2, z1:z2, y1:y2, x1:x2])

                    if dimsSelectedRegions == [0]:  #if time is region
                        self._exportCaption = '{0}__GridZ_timeseries__GridX={1}_GridY={2}_GridZ={3}'.format(self.get_current_var_name(), x1, y1, z1)
                    elif dimsSelectedRegions == [1]:  #if z is region
                        self._exportCaption = '{0}__GridZ__timestep={3}_GridX={1}_GridY={2}'.format(self.get_current_var_name(), x1, y1, t1)
                    elif dimsSelectedRegions == [2]:  # if y is region
                        self._exportCaption = '{0}__GridZ__timestep={1}_GridX={2}_GridZ={3}'.format(self.get_current_var_name(), t1, x1, z1)
                    elif dimsSelectedRegions == [3]:  # if y is region
                        self._exportCaption = '{0}__GridZ_timeseries__timestep={2}_GridX={1}_GridZ={3}'.format(self.get_current_var_name(), x1, t1, z1)

                elif ndims == 3:
                    if missingDimList == [0]:  # time is missing
                        z = squeeze(var[z1:z2, y1:y2, x1:x2])
                        if dimsSelectedRegions == [1]:  #if z is region
                            self._exportCaption = '{0}__GridX={1}_GridY={2}'.format(self.get_current_var_name(), x1, y1)
                        elif dimsSelectedRegions == [2]:  #if y is region
                            self._exportCaption = '{0}__GridX={1}_GridZ={2}'.format(self.get_current_var_name(), x1, z1)
                        elif dimsSelectedRegions == [3]:  # if x  is region
                            self._exportCaption = '{0}__GridY={1}_GridZ={2}'.format(self.get_current_var_name(), y1, z1)

                    elif missingDimList == [1]:  # z is missing
                        z = squeeze(var[t1:t2, y1:y2, x1:x2])
                        if dimsSelectedRegions == [0]:  #if time is region
                            self._exportCaption = '{0}__timeseries_GridX={1}_GridY={2}'.format(self.get_current_var_name(), x1, y1)
                        elif dimsSelectedRegions == [2]:  #if y is region
                            self._exportCaption = '{0}__timestep={2}_GridX={1}'.format(self.get_current_var_name(), x1, t1)
                        elif dimsSelectedRegions == [3]:  # if x  is region
                            self._exportCaption = '{0}__timestep={2}_GridY={1}'.format(self.get_current_var_name(), y1, t1)

                    elif missingDimList == [2]:  # y is missing
                        z = squeeze(var[t1:t2, z1:z2, x1:x2])
                        if dimsSelectedRegions == [0]:  #if time is region
                            self._exportCaption = '{0}__timeseries_GridX={1}_GridZ={2}'.format(self.get_current_var_name(), x1, z1)
                        elif dimsSelectedRegions == [1]:  #if z is region
                            self._exportCaption = '{0}__timestep={2}_GridX={1}'.format(self.get_current_var_name(), x1, t1)
                        elif dimsSelectedRegions == [3]:  # if x  is region
                            self._exportCaption = '{0}__timestep={2}_GridZ={1}'.format(self.get_current_var_name(), z1, t1)

                    elif missingDimList == [3]:  # x is missing
                        z = squeeze(var[t1:t2, z1:z2, y1:y2])
                        if dimsSelectedRegions == [0]:  #if time is region
                            self._exportCaption = '{0}__timeseries_GridY={1}_GridZ={2}'.format(self.get_current_var_name(), y1, z1)
                        elif dimsSelectedRegions == [1]:  #if z is region
                            self._exportCaption = '{0}__timestep={2}_GridY={1}'.format(self.get_current_var_name(), y1, t1)
                        elif dimsSelectedRegions == [2]:  # if y  is region
                            self._exportCaption = '{0}__timestep={2}_GridZ={1}'.format(self.get_current_var_name(), z1, t1)

                elif ndims == 2:
                    if missingDimList == [0, 1]:  # time, z are missing
                        z = squeeze(var[y1:y2, x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [0, 2]:  # time, y are missing
                        z = squeeze(var[z1:z2, x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [0, 3]:  # time, x are missing
                        z = squeeze(var[z1:z2, y1:y2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [1, 2]:  # z, y are missing
                        z = squeeze(var[t1:t2, x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [1, 3]:  # z, x are missing
                        z = squeeze(var[t1:t2, y1:y2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [2, 3]:  # y, x are missing
                        z = squeeze(var[t1:t2, z1:z2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())



                elif ndims == 1:
                    if presentDimList == [0]:  # timeline
                        z = squeeze(var[t1:t2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    elif presentDimList == [1]:  # z-line
                        z = squeeze(var[z1:z2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    elif presentDimList == [2]:  # y-line
                        z = squeeze(var[y1:y2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    elif presentDimList == [3]:  # x-line
                        z = squeeze(var[x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    #if t1 is None:

            elif sum(isSelectedRegion) == 0:
                if detect_plot_type: self._plotType = '0D'
                if ndims == 4:
                    z = squeeze(var[t1:t2, z1:z2, y1:y2, x1:x2])
                    self._exportCaption = '{0}__timestep={4}__GridX={1}_GridY={2}_GridZ={3}'.format(self.get_current_var_name(), x1, y1, z1, t1)

                elif ndims == 3:
                    if missingDimList == [0]:  # time is missing
                        z = squeeze(var[z1:z2, y1:y2, x1:x2])
                        self._exportCaption = '{0}___GridX={3}_GridY={1}_GridZ={2}'.format(self.get_current_var_name(), y1, z1, x1)
                    elif missingDimList == [1]:  # z is missing
                        z = squeeze(var[t1:t2, y1:y2, x1:x2])
                        self._exportCaption = '{0}___timestep={2}_GridX={3}_GridY={1}'.format(self.get_current_var_name(), y1, t1, x1)
                    elif missingDimList == [2]:  # y is missing
                        z = squeeze(var[t1:t2, z1:z2, x1:x2])
                        self._exportCaption = '{0}___timestep={2}_GridX={3}_GridZ={1}'.format(self.get_current_var_name(), z1, t1, x1)

                    elif missingDimList == [3]:  # x is missing
                        z = squeeze(var[t1:t2, z1:z2, y1:y2])
                        self._exportCaption = '{0}___timestep={2}_GridY={3}_GridZ={1}'.format(self.get_current_var_name(), z1, t1, y1)

                elif ndims == 2:
                    if missingDimList == [0, 1]:  # time, z are missing
                        z = squeeze(var[y1:y2, x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [0, 2]:  # time, y are missing
                        z = squeeze(var[z1:z2, x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [0, 3]:  # time, x are missing
                        z = squeeze(var[z1:z2, y1:y2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [1, 2]:  # z, y are missing
                        z = squeeze(var[t1:t2, x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [1, 3]:  # z, x are missing
                        z = squeeze(var[t1:t2, y1:y2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    if missingDimList == [2, 3]:  # y, x are missing
                        z = squeeze(var[t1:t2, z1:z2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())

                elif ndims == 1:
                    if presentDimList == [0]:  # timeline
                        z = squeeze(var[t1:t2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    elif presentDimList == [1]:  # z-line
                        z = squeeze(var[z1:z2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    elif presentDimList == [2]:  # y-line
                        z = squeeze(var[y1:y2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())
                    elif presentDimList == [3]:  # x-line
                        z = squeeze(var[x1:x2])
                        self._exportCaption = '{0}'.format(self.get_current_var_name())




            if self._plotType == "2D":
                if dimsSelectedRegions == [0, 1]:  #if time, z are regions
                    if detect_plot_type: self.plotType_for_timeseries = "2DZT"
                elif dimsSelectedRegions == [0, 2]:  #if time, y are regions
                    if detect_plot_type: self.plotType_for_timeseries = "2DYT"
                elif dimsSelectedRegions == [0, 3]:  #if time, x are regions
                    if detect_plot_type: self.plotType_for_timeseries = "2DXT"
                elif dimsSelectedRegions == [1, 2]:  #if z, y are regions
                    if detect_plot_type: self.plotType_for_timeseries = '2DZY'
                elif dimsSelectedRegions == [1, 3]:  #if z, x are regions
                    if detect_plot_type: self.plotType_for_timeseries = '2DZX'
                elif dimsSelectedRegions == [2, 3]:  # if x,y are regions
                    if detect_plot_type: self.plotType_for_timeseries = '2DXY'
            elif self._plotType == '1D':
                if dimsSelectedRegions == [0]:  #if time is region
                    if detect_plot_type: self.plotType_for_timeseries = '1DT'
                elif dimsSelectedRegions == [1]:  #if z is region
                    if detect_plot_type: self.plotType_for_timeseries = '1DZ'
                elif dimsSelectedRegions == [2]:  # if y is region
                    if detect_plot_type: self.plotType_for_timeseries = '1DY'
                elif dimsSelectedRegions == [3]:  # if x is region
                    if detect_plot_type: self.plotType_for_timeseries = '1DX'

            #print 'selected plotType is: ', self._plotType
            #print 'selected plotType_for_timeseries is: ', self.plotType_for_timeseries
            #print 'Shape of the array to plot.... >>> ', z.shape
        return z, dimsSelectedRegions


    def imshow_setData(self, msg=''):
        #background = [self.pltWidget.canvas.copy_from_bbox(self.pltWidget.get_axes()[0].bbox)]


        print '+++++++++++ t={0} , starting imshow_setData()++++++++++++'.format(msg)
        start1 = time.clock()
        start = time.clock()
        nc = self.get_selected_ncfile_instance()
        end = time.clock(); print '{0:.6f}'.format(end - start), 'opened netcdf > seconds:'
        start = time.clock()
        var = nc.variables[self.get_current_var_name()]
        end = time.clock(); print '{0:.6f}'.format(end - start), 'loaded var > seconds:'

        start = time.clock()
        t1, t2, z1, z2, y1, y2, x1, x2, ndims, isSelectedRegion, isSelectedPoint, dimensionIsPresent = self.get_values_to_select_data()
        end = time.clock(); print '{0:.6f}'.format(end - start), 'get_values_to_select_data() > seconds:'
        start = time.clock()
        z, dimsSelectedRegions = self.detectPlotType_andReturnData(var)
        end = time.clock(); print '{0:.6f}'.format(end - start), 'detectPlotType_andReturnData() > seconds:'
        start = time.clock()
        nc.close()
        end = time.clock(); print '{0:.6f}'.format(end - start), 'closing file() > seconds:'
        end = time.clock(); print '{0:.6f}'.format(end - start1), '... so far > seconds:'




        if self._plotType == '2D':
            start = time.clock()
            z = np.flipud(z)  # flip array vertically
            end = time.clock(); print '{0:.6f}'.format(end - start), 'flipping array > seconds:'
            
            start = time.clock()
            # apply mouse hover values
            self.pltWidget.change_coordinate_formatter(self.plot_setup['ax'], z, bruteforce_flag=self.plotType_for_timeseries, bruteforce_dims=self.plot_setup['REAL_DIMS'])
            end = time.clock(); print '{0:.6f}'.format(end - start), 'apply mouse hover values > seconds:'
            
            start = time.clock()
            # set new data to imshow
            self.plot_setup['im'].set_data(z)
            end = time.clock(); print '{0:.6f}'.format(end - start), 'imshow.set_data() > seconds:'
            #print 'defaukt range cbar:', self.cb.isDefaultRange()

            start = time.clock()
            self.pltWidget.fast_redraw(self.plot_setup['im'])
            #self.pltWidget.canvas.draw()
            end = time.clock(); print '{0:.6f}'.format(end - start), 'self.pltWidget.canvas.update() NEW NEW > seconds:'
            

            start = time.clock()
            self.tableView.setArrayData(z)
            end = time.clock(); print '{0:.6f}'.format(end - start), 'tableView.setArrayData() > seconds:'
            end = time.clock(); print '{0:.6f}'.format(end - start1), 'altogether() > seconds:'
        elif self._plotType == '1D':
            self.plot2D_hardcode()
        print '+++++++++++ t={0} , finishing imshow_setData()+++++++++++++'.format(msg)
        print '### FINISHED t= {1}, FPS: {0} ###'.format(1./(end-start1), msg)




    def plot2D_hardcode(self, cb_min=None, cb_max=None):
        print 'running plot2D'
        try: self.pltWidget.clear_selection()
        except Exception as e: print e




        nc = self.get_selected_ncfile_instance()
        var = nc.variables[self.get_current_var_name()]

        t1, t2, z1, z2, y1, y2, x1, x2, ndims, isSelectedRegion, isSelectedPoint, dimensionIsPresent = self.get_values_to_select_data()
        z, dimsSelectedRegions = self.detectPlotType_andReturnData(var)

        lon_label = 'GridX'
        lat_label = 'GridY'
        time_label = 'Grid Time'

        if self._plotType is not None:  # if correct ploting type was chosen

            f = self.pltWidget.fig  # pointer to matplotwidget figure
            f.clf(keep_observers=True)
            #f = self.pltWidget.fig.add_subplot(111, label=self.get_current_var_name() )
            ax = f.add_subplot(111)
            ax.set_title(self.get_current_var_name())

            f.set_tight_layout(True)

            self.spinBox_Timestep.setEnabled(False)
            self.pushButton_playTime.setEnabled(False)
            self.spinBox_GridZ.setEnabled(False)
            self.pltWidget.grid_cb.setEnabled(True)
            self.pltWidget.mpl_toolbar.setEnabled(True)

            if self.__tIsPresent and (0 not in dimsSelectedRegions):
                self._doNotPlotSecondTime = True
                self.spinBox_Timestep.setValue(self.get_selectedT())

            if self.__zIsPresent and (1 not in dimsSelectedRegions):
                self._doNotPlotSecondTime = True
                self.spinBox_GridZ.setValue(int(self.lineEdit_z.text()))
            self._doNotPlotSecondTime = False
            #self.spinBox_Timestep.setEnabled(self.__tIsPresent)
            #self.spinBox_GridZ.setEnabled(self.__zIsPresent)

            # --------------------------------------------------------------
            # ----                2D                        ----------------
            # --------------------------------------------------------------
            if self._plotType == '2D':
                self.pushButton_cbarRange.setEnabled(True)

                z = np.flipud(z)  # flip array vertically
                REAL_DIMS = self.detect_variable_dimensions(log=True)


                if self.plotType_for_timeseries == "2DZT":  #if time, z are regions
                    z = z.T
                    z = np.flipud(z)  # flip array vertically
                    z = np.fliplr(z)  # flip array horizontally

                    im = ax.imshow(z, interpolation='nearest', cmap=cm.gist_rainbow,  aspect='auto', vmin=cb_min, vmax=cb_max)
                    #t1, t2 = set_x_axis_as_time(ax, t1, t2, time_label, dateFormat=dtFmt)
                    ax.set_xlabel(time_label)
                    ax.set_ylabel('grid Z')

                elif self.plotType_for_timeseries == "2DYT":  #if time, y are regions
                    pass
                elif self.plotType_for_timeseries == "2DXT":  #if time, x are regions
                    pass
                elif self.plotType_for_timeseries == '2DZY':  #if z, y are regions
                    im = ax.imshow(z, extent=(y1-.5, y2-1.5, z1-.5, z2-1.5), interpolation='nearest', cmap=cm.gist_rainbow, aspect='auto', vmin=cb_min, vmax=cb_max)
                    ax.set_xlabel(lat_label)
                    ax.set_ylabel('grid Z')

                    self.spinBox_Timestep.setEnabled(self.__tIsPresent)
                    self.pushButton_playTime.setEnabled(self.__tIsPresent)

                elif self.plotType_for_timeseries == '2DZX':  #if z, x are regions
                    im = ax.imshow(z, extent=(x1-.5, x2-1.5, z1-.5, z2-1.5), interpolation='nearest', cmap=cm.gist_rainbow, aspect='auto', vmin=cb_min, vmax=cb_max)
                    ax.set_xlabel(lon_label)
                    ax.set_ylabel('grid Z')

                    self.spinBox_Timestep.setEnabled(self.__tIsPresent)
                    self.pushButton_playTime.setEnabled(self.__tIsPresent)

                elif self.plotType_for_timeseries == '2DXY':  # if x,y are regions
                    im = ax.imshow(z, extent=(x1-.5, x2-1.5, y1-.5, y2-1.5), interpolation='nearest', cmap=cm.gist_rainbow,  aspect='auto', vmin=cb_min, vmax=cb_max)
                    ax.set_xlabel(lon_label)
                    ax.set_ylabel(lat_label)

                    self.spinBox_GridZ.setEnabled(self.__zIsPresent)
                    self.spinBox_Timestep.setEnabled(self.__tIsPresent)
                    self.pushButton_playTime.setEnabled(self.__tIsPresent)




                # ----------------------------------------------------------------
                # restore COLORBAR and ZOOM view
                # ----------------------------------------------------------------

                # now init new cb
                self.cb = f.colorbar(im)
                self.cb = DraggableColorbar(self.cb, im, parent=self)

                self.pltWidget.change_coordinate_formatter(ax, z, bruteforce_flag=self.plotType_for_timeseries, bruteforce_dims=REAL_DIMS)
                self.plot_setup = dict()
                self.plot_setup['im'] = im  # pointer to this figure to change it in future
                self.plot_setup['ax'] = ax
                self.plot_setup['REAL_DIMS'] = REAL_DIMS
                try:
                    self.cb.set_label(var.units)
                except:
                    pass
            # --------------------------------------------------------------
            # ----                1D                        ----------------
            # --------------------------------------------------------------
            elif self._plotType == '1D':
                ax.plot(z)

                if self.plotType_for_timeseries == '1DT':  #if time is region
                    ax.set_xlabel('Grid Time')
                    self.spinBox_GridZ.setEnabled(self.__zIsPresent)
                    self.pushButton_playTime.setEnabled(self.__tIsPresent)

                elif self.plotType_for_timeseries == '1DZ':  #if z is region
                    ax.set_xlabel('grid Z')
                    self.spinBox_Timestep.setEnabled(self.__tIsPresent)
                    self.pushButton_playTime.setEnabled(self.__tIsPresent)

                elif self.plotType_for_timeseries == '1DY':  # if y is region
                    ax.set_xlabel(lat_label)
                    self.spinBox_Timestep.setEnabled(self.__tIsPresent)
                    self.pushButton_playTime.setEnabled(self.__tIsPresent)
                    self.spinBox_GridZ.setEnabled(self.__zIsPresent)

                elif self.plotType_for_timeseries == '1DX':  # if x is region
                    ax.set_xlabel(lon_label)
                    self.spinBox_Timestep.setEnabled(self.__tIsPresent)
                    self.pushButton_playTime.setEnabled(self.__tIsPresent)
                    self.spinBox_GridZ.setEnabled(self.__zIsPresent)

                try:
                    ax.set_ylabel(var.units)
                except:
                    pass

            # --------------------------------------------------------------
            # ----                0D                        ----------------
            # --------------------------------------------------------------
            elif self._plotType == '0D':
                try:
                    varunits = var.units
                except AttributeError:
                    varunits = ''
                ax.text(0.2, 0.5, '{0} {1}'.format(z, varunits), fontsize=10)


            self.pltWidget.canvas.draw()
            self.tableView.setArrayData(z)
            if self.tabWidget.currentWidget() in [self.tab]:
                self.tabWidget.setCurrentWidget(self.tab_3)

        del(var)
        nc.close()
        del(nc)
        self.updateUi(DoNotResetTableView=True)



"""
def main():
    app = QApplication(sys.argv)
    app.setOrganizationName("kkk")
    app.setOrganizationDomain("kkk")
    app.setApplicationName("kkk")
    form = selectdata_dlg(path=os.path.dirname(sys.argv[0]))
    form.show()
    app.exec_()
    form.closeFile()

if __name__ == "__main__":
    main()
"""
