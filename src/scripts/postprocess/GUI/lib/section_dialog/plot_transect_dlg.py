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


from PyQt4.QtCore import QString, pyqtSignature
from PyQt4.QtGui import QDialog, QMessageBox, QFileDialog
import ui_plot_transect_dlg
import netCDF4
import numpy as np


import inspect, os, sys
relative_paths = [

    '../plot',
    '../section_plot_dialog',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

import function_plot_GF_transect_coord as plot_transect_1D
import plot_2D_crossection_coord as plot_transect_2D
from crossection_dlg import Mpl_dlg


class transection_dlg_data_collector():
    def __init__(self, point1=None, point2=None, bathymetry_fname=None, vector_length=100):
        '''
        point1 = [lat, lon]
        point2 = [lat, lon]
        bathymetry_fname = 'string'
        '''

        self.point1 = point1
        self.point2 = point2
        self.bathymetry_fname = bathymetry_fname
        self.vector_length = 100  # default length of vector > 100 points

    def set_p1(self, p1):
        ''' point1 = [lat, lon] '''
        self.point1 = p1

    def set_p2(self, p2):
        ''' point2 = [lat, lon] '''
        self.point2 = p2

    def set_bath(self, string):
        self.bathymetry_fname = string

    def set_vector_length(self, val):
        self.vector_length = int(val)

    # -----------------------------------------------
    def get_p1(self):
        return self.point1

    def get_p2(self):
        return self.point2

    def get_bath(self):
        return self.bathymetry_fname

    def get_vector_length(self):
        return self.vector_length

    def get_status(self):
        return [bool(self.point1), bool(self.point2), bool(self.bathymetry_fname)]


class My_dlg(QDialog, ui_plot_transect_dlg.Ui_Dialog):
    def __init__(self, parent=None):
        super(My_dlg, self).__init__(parent)
        self.parent = parent
        self.setupUi(self)

        self.toogle_show_after_selected_cell = False

        self.data = transection_dlg_data_collector()
        self.set_toolTips()


    def clear_data(self):
        # at the end nulify this object

        del self.data
        self.data = None
        self.toogle_show_after_selected_cell = False

    def set_toolTips(self):
        self.lineEdit_lat1.setToolTip('Latitude in <b>decimal degrees</b> for point1 of crossection')
        self.lineEdit_lon1.setToolTip('Longitude in <b>decimal degrees</b> for point1 of crossection')
        self.lineEdit_lat2.setToolTip('Latitude in <b>decimal degrees</b> for point2 of crossection')
        self.lineEdit_lon2.setToolTip('Longitude in <b>decimal degrees</b> for point2 of crossection')
        self.pushButton_pick1.setToolTip('Click on the map with <b>RMB</b> to pick point1')
        self.pushButton_pick2.setToolTip('Click on the map with <b>RMB</b> to pick point2')
        self.pushButton_selectBathymetry.setToolTip('Select the netCDF file with <b>bathymetry</b> variable corresponding current setup')
        self.lineEdit_nVector.setToolTip('Should be integer >= 2 <br> This value represents number of data point in ' +
                'section-line that will be created (including two side points). If N=100 points, the data will be interpolated into 100 equally spaced points between ' +
                'user defined beginning and end of the section (Point1, Point2)')


    def update(self):
        self.toogle_show_after_selected_cell = False
        if self.data.get_p1():
            #print 'setting lat1:', self.data.get_p1()[1]
            self.lineEdit_lat1.setText(QString(str(self.data.get_p1()[0])))
        if self.data.get_p1():
            #print 'setting lon1:', self.data.get_p1()[0]
            self.lineEdit_lon1.setText(QString(str(self.data.get_p1()[1])))
        if self.data.get_p2():
            #print 'setting lat2:', self.data.get_p2()[1]
            self.lineEdit_lat2.setText(QString(str(self.data.get_p2()[0])))
        if self.data.get_p2():
            #print 'setting lon2:', self.data.get_p2()[0]
            self.lineEdit_lon2.setText(QString(str(self.data.get_p2()[1])))
        if self.data.get_bath():
            #print 'setting bath:', self.data.get_bath()
            self.lineEdit_bathymetry.setText(QString(self.data.get_bath()))
        if self.data.get_vector_length():
            #print 'setting bath:', self.data.get_bath()
            self.lineEdit_nVector.setText(QString(str(self.data.get_vector_length())))

    @pyqtSignature("QString")
    def on_lineEdit_nVector_textChanged(self, new_str):
        try:
            new_val = int(str(new_str))
            if new_val > 2:
                self.data.set_vector_length(new_val)
            else:
                QMessageBox.question(self, 'Error', 'Vector consists minimum of two points! Did not you know that?', QMessageBox.Ok)
                self.lineEdit_nVector.setText(QString(str(self.data.get_vector_length())))
        except:
            QMessageBox.question(self, 'Error', 'Should be integer >= 2 <br> This value represents number of data point in ' +
                'selected section-line. If N=100 points, the data will be interpolated into 100 equally spaced points between ' +
                'user defined beginning and end of the section (Point1, Point2)', QMessageBox.Ok)
            self.lineEdit_nVector.setText(QString(str(self.data.get_vector_length())))


    @pyqtSignature("QString")
    def on_lineEdit_lat1_textChanged(self, new_str):
        print 'lat1 textChanged >>>', str(new_str)
        try:
            new_val = float(str(new_str))
            if self.data.get_p1():
                self.data.set_p1([new_val, self.data.get_p1()[1]])
            else:
                self.data.set_p1([None, new_val])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_lat1.setText(QString(str(self.data.get_p1()[0])))


    @pyqtSignature("QString")
    def on_lineEdit_lon1_textChanged(self, new_str):
        try:
            new_val = float(str(new_str))
            if self.data.get_p1():
                self.data.set_p1([self.data.get_p1()[0], new_val])
            else:
                self.data.set_p1([new_val, None])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_lon1.setText(QString(str(self.data.get_p1()[1])))

    @pyqtSignature("QString")
    def on_lineEdit_lat2_textChanged(self, new_str):
        try:
            new_val = float(str(new_str))
            if self.data.get_p2():
                self.data.set_p2([new_val, self.data.get_p2()[1]])
            else:
                self.data.set_p2([None, new_val])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_lat2.setText(QString(str(self.data.get_p2()[0])))


    @pyqtSignature("QString")
    def on_lineEdit_lon2_textChanged(self, new_str):
        try:
            new_val = float(str(new_str))
            if self.data.get_p2():
                self.data.set_p2([self.data.get_p2()[0], new_val])
            else:
                self.data.set_p2([new_val, None])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_lon2.setText(QString(str(self.data.get_p2()[1])))

    @pyqtSignature("")
    def on_pushButton_selectBathymetry_clicked(self):
        openpath = os.path.dirname(self.parent.filename)
        fname = unicode(QFileDialog.getOpenFileName(self, 'Open data file', openpath, filter='NetCDF files (*.nc)'))
        if fname:
            f = netCDF4.Dataset(fname, mode='r')
            bathymetry_var_name = 'bathymetry'

            if bathymetry_var_name in f.variables.keys():
                nc = self.parent.get_selected_ncfile_instance()
                current_nc_var = nc.variables[str(self.parent.comboBox_dataType.currentText())]
                data , _ = self.parent.detectPlotType_andReturnData(current_nc_var, detect_plot_type=False)
                bathymetry_data = f.variables[bathymetry_var_name]
                

                # now check dimensions of bathymetry and data!
                if data.shape == bathymetry_data.shape:
                    self.lineEdit_bathymetry.setText(QString(fname))
                    self.data.set_bath(fname)
                else:
                    QMessageBox.question(self, 'Error', str('ERROR! Data-array shape {0} differes from bathymetry-array shape {1}. Must be same.'.format(data.shape, bathymetry_data.shape)), QMessageBox.Ok)
                nc.close()
            else:
                QMessageBox.question(self, 'Error', str('ERROR! Variable "{0}"" not found in file {1}'.format(bathymetry_var_name, fname)), QMessageBox.Ok)
            f.close()



    @pyqtSignature("")
    def on_pushButton_pick1_clicked(self):
        self.toogle_show_after_selected_cell = 1
        self.hide()

    def on_pushButton_pick2_clicked(self):
        self.toogle_show_after_selected_cell = 2
        self.hide()


    def accept(self):
        if all(self.data.get_status()):  #if all data is fullfilled
            self.update()  # saving current data
            nc = self.parent.get_selected_ncfile_instance()
            # open file , get data whole data
            array_length = self.data.get_vector_length()
            current_nc_var = nc.variables[self.parent.get_current_var_name()]
            selected_data , _ = self.parent.detectPlotType_andReturnData(current_nc_var, detect_plot_type=False)
            crd1 = self.data.get_p1()
            crd2 = self.data.get_p2()
            fname = self.parent.get_current_nc_fname()
            fname_topo = self.data.get_bath()
            varname = self.parent.get_current_var_name()
            clim = [self.parent.cb.get_min(), self.parent.cb.get_max()]


            # enable gui, plot
            gui = Mpl_dlg()
            ax1, ax2 = gui.get_axes()



            # if 2D
            if len(selected_data.shape) == 2 and not self.parent.get_zIsPresent():
                plot_transect_1D.main(crd1, crd2, fname, fname_topo, varname, selected_data, arlen=array_length, PRJ_DATA=None, clim=clim, ax1=ax1, ax2=ax2)
                gui.redraw()
                gui.show()
                # close this dialog
                self.done(1)


            # if 3D
            elif len(selected_data.shape) == 2 and self.parent.get_zIsPresent():
                if len(current_nc_var.shape) == 4:
                    selected_data = current_nc_var[ self.parent.get_selectedT() , :, :, :]  # >>> reselecting 3D var
                if len(current_nc_var.shape) == 3:
                    selected_data = current_nc_var[:, :, :]  # >>> reselecting 3D var



                # artificial_data
                _2D_bathymetry = plot_transect_2D.getvar_ffile_old(fname_topo, 'bathymetry')
                n_layers = selected_data.shape[0]
                layerHeight3D = np.tile(_2D_bathymetry, (n_layers+1, 1, 1))
                for l in xrange(n_layers):
                    layerHeight3D[l, ...] = layerHeight3D[l, ...]*(float(n_layers-l)/float(n_layers))
                layerHeight3D[-1, ...] = 0.


                plot_transect_2D.main(crd1, crd2, fname, fname_topo, varname, selected_data, layerHeight3D, arlen=array_length, PRJ_DATA=None, clim=clim, ax1=ax1, ax2=ax2)

                nc.close()
                gui.redraw()
                gui.show()
                # close this dialog
                self.done(1)
            else:
                QMessageBox.question(self, 'Error', 'Selected variable is not 2D or 3D. Cannot plot transection', QMessageBox.Ok)

        else:
            pass

    def reject(self):
        self.clear_data()
        super(My_dlg, self).reject()

