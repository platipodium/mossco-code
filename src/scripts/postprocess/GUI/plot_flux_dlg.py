from PyQt4.QtCore import QString, pyqtSignature
from PyQt4.QtGui import QDialog, QMessageBox, QFileDialog
import ui_plot_flux_dlg

import netCDF4
import numpy as np

import inspect, os, sys
relative_paths = [

    '../plot',
    '../flux_widget',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

from flux_tab_widget import plot_flux_widget_dlg as tab_widget_flux


class flux_dlg_data_collector():
    def __init__(self, parent=None):
        '''
        point1 = [i, j]
        point2 = [i, j]
        bathymetry_fname = 'string'
        '''
        self.parent = parent
        self.point1 = [0, 0]
        self.point2 = [0, 0]
        self.bathymetry_fname = None
        self.dlat_dlon = None
        self.lonVar = None
        self.latVar = None
        self.latNcVar = None
        self.lonNcVar = None

    def set_p1(self, p1):
        ''' point1 = [i, j] '''
        self.point1 = p1

    def set_p2(self, p2):
        ''' point2 = [i, j] '''
        self.point2 = p2

    def set_bath(self, string):
        self.bathymetry_fname = string

    def set_dlat_dlon(self, dlat, dlon):
        self.dlat_dlon = [dlat, dlon]

    def set_lonVar(self, string):
        self.lonVar = str(string)
    def set_latVar(self, string):
        self.latVar = str(string)
    def set_lonNcVar(self, lon_var):
        self.lonNcVar = lon_var
    def set_latNcVar(self, lat_var):
        self.latNcVar = lat_var
    # -----------------------------------------------
    def get_p1(self):
        return self.point1
    def get_p2(self):
        return self.point2
    def get_status(self):
        return [bool(self.point1), bool(self.point2)]
    def get_bath(self):
        return self.bathymetry_fname
    def get_dlat_dlon(self):
        return self.dlat_dlon
    def get_lonVar(self):
        return self.lonVar
    def get_latVar(self):
        return self.latVar
    def get_lonNcVar(self):
        return self.lonNcVar
    def get_latNcVar(self):
        return self.latNcVar
    
    def get_nc_varlist(self):
        nc = self.parent.parent.get_selected_ncfile_instance()
        l = list()
        for var in nc.variables.keys():
            l.append(unicode(var))

        nc.close()
        return l
    
    def get_nc_name(self):
        return str(self.parent.parent.comboBox_file.currentText())
    def get_max_t(self):
        return self.parent.parent.get_nTimesteps()
    def get_current_t(self):
        try:
            t = int(self.parent.parent.lineEdit_t.text())
        except:
            t = None
        print 'Current t is :', t
        return t


class My_dlg(QDialog, ui_plot_flux_dlg.Ui_Dialog):
    def __init__(self, parent=None):
        super(My_dlg, self).__init__(parent)
        self.parent = parent
        self.setupUi(self)

        self.toogle_show_after_selected_cell = False

        self.data = flux_dlg_data_collector(self)
        self.set_toolTips()
        self.get_grid_info(self.data.get_nc_name())
        self.init_comboboxes()


    def clear_data(self):
        # at the end nulify this object
        del self.data
        self.data = None
        self.toogle_show_after_selected_cell = False

    def set_toolTips(self):
        self.lineEdit_i1.setToolTip('Cell-i <b>integer</b> for point1 of crossection')
        self.lineEdit_j1.setToolTip('Cell-j <b>integer</b> for point1 of crossection')
        self.lineEdit_i2.setToolTip('Cell-i <b>integer</b> for point2 of crossection')
        self.lineEdit_j2.setToolTip('Cell-j <b>integer</b> for point2 of crossection')
        self.pushButton_pick1.setToolTip('Click on the map with <b>RMB</b> to pick point1')
        self.pushButton_pick2.setToolTip('Click on the map with <b>RMB</b> to pick point2')



    def update(self):


        self.toogle_show_after_selected_cell = False
        if self.data.get_p1():
            self.lineEdit_i1.setText(QString(str(self.data.get_p1()[0])))
        if self.data.get_p1():
            self.lineEdit_j1.setText(QString(str(self.data.get_p1()[1])))
        if self.data.get_p2():
            self.lineEdit_i2.setText(QString(str(self.data.get_p2()[0])))
        if self.data.get_p2():
            self.lineEdit_j2.setText(QString(str(self.data.get_p2()[1])))
        if self.data.get_dlat_dlon()[0] and self.data.get_dlat_dlon()[1]:
            self.label_6.setText(QString('cell size (lat, lon) = ({0}, {1})'.format(self.data.get_dlat_dlon()[0], self.data.get_dlat_dlon()[1])))
            self.lineEdit_bathymetry.setText(QString(str(self.data.get_bath())))
        if self.data.get_latVar():
            self.label_latVar.setText(QString('Lat variable = {0}'.format(self.data.get_latVar()) ))
        if self.data.get_lonVar():
            self.label_lonVar.setText(QString('Lon variable = {0}'.format(self.data.get_lonVar()) ))


    def init_comboboxes(self):
        for cb in [self.comboBox_waterDepth, self.comboBox_relativeLayerThickness, self.comboBox_velx, self.comboBox_vely, self.comboBox_spm]:
            self.populate_combobox_with_variables(cb)
        
        self.find_var_in_comboboxes()


    def find_var_in_comboboxes(self):
        wd_found = False
        rlt_found = False
        vx_found = False
        vy_found = False
        spm_found = False

        var_list = self.data.get_nc_varlist()
        
        #comboBox_waterDepth
        wd_varnames = ['water_depth_at_soil_surface']
        for wd_varname in wd_varnames:
            if wd_varname in var_list:
                index = var_list.index(wd_varname)
                self.comboBox_waterDepth.setCurrentIndex(index)
                wd_found = True
                break

        #comboBox_relativeLayerThickness
        rlt_varnames = ['relative_layer_thickness']
        for rlt_varname in rlt_varnames:
            if rlt_varname in var_list:
                index = var_list.index(rlt_varname)
                self.comboBox_relativeLayerThickness.setCurrentIndex(index)
                rlt_found = True
                break


        #comboBox_velx
        velx_varnames = ['x_velocity_in_water']
        for velx_varname in velx_varnames:
            if velx_varname in var_list:
                index = var_list.index(velx_varname)
                self.comboBox_velx.setCurrentIndex(index)
                vx_found = True
                break
        
        #comboBox_vely
        vely_varnames = ['y_velocity_in_water']
        for vely_varname in vely_varnames:
            if vely_varname in var_list:
                index = var_list.index(vely_varname)
                self.comboBox_vely.setCurrentIndex(index)
                vy_found = True
                break

        #comboBox_spm
        spm_varnames = 'concentration_of_SPM_in_water_001', 'concentration_of_SPM_in_water_002'
        for spm_varname in spm_varnames:
            if spm_varname in var_list:
                index = var_list.index(spm_varname)
                self.comboBox_spm.setCurrentIndex(index)
                spm_found = True
                break
        found_list = [wd_found, rlt_found, vx_found, vy_found, spm_found]
        if not all(found_list):
            msg = 'Following variables not found. Select manually:'
            for item, name in zip(found_list, ['Water Depth', 'Relative Layer Thickness' 'Velocity X', 'Velocity Y', 'SPM']):
                if not item:
                    msg = msg+'<br>  {0}'.format(name)
            QMessageBox.question(self, 'Warning', msg, QMessageBox.Ok)


    def populate_combobox_with_variables(self, combobox):
        combobox.clear()
        try:
            for var in self.data.get_nc_varlist():
                combobox.addItem(unicode(var))
        except:
            raise TypeError("Cannot find variables in NC file. Check if the file is damaged")


    def validate_input(self):
        VALID = dict()
        VALID['value'] = True

        #Validate coordinates
        try:
            i1, j1 = self.data.get_p1()
            i2, j2 = self.data.get_p2()
            print 'point 1:', i1, j1
            print 'point 2:', i2, j2
            if not ((i1 == i2) or (j1 == j2)):
                VALID['value'] = False
                VALID['error1'] = 'Two points are not on horizontal or verticall line.'
        except:
            VALID['value'] = False
            VALID['error0'] = 'Not all points were specified'


        # validate lat/lon
        if self.data.get_dlat_dlon() is None:
            VALID['value'] = False
            VALID['error2'] = 'lon/lat inforamtion not specified'
        
        # validate variables
        nc = self.parent.get_selected_ncfile_instance()
        shape_wd = nc.variables[str(self.comboBox_waterDepth.currentText())].shape
        shape_rlt = nc.variables[str(self.comboBox_relativeLayerThickness.currentText())].shape
        shape_velx    = nc.variables[str(self.comboBox_velx.currentText())].shape
        shape_vely    = nc.variables[str(self.comboBox_vely.currentText())].shape
        shape_spm     = nc.variables[str(self.comboBox_spm.currentText())].shape
        nc.close()
        if shape_spm != shape_vely != shape_velx:
            VALID['value'] = False
            VALID['error3'] = 'Shapes of the selected <b>VelX</b> ({0}), <b>VelY</b> ({1}), <b>SPM</b> ({2}) variables are not equal'.format(shape_velx, shape_vely, shape_spm)

        try:
            correct_shape_wd = tuple((shape_velx[0], shape_velx[2], shape_velx[3]))
            if shape_wd != correct_shape_wd:
                VALID['value'] = False
                VALID['error4'] = 'Shapes of the selected <b>Water Depth</b> is not correct. Expected: {0}. Got: {1}'.format(correct_shape_wd, shape_wd)
        except Exception as e:
            print e
            VALID['value'] = False
            VALID['error5'] = 'Set inputs to <b>Water Depth</b> and <b>Velocity X</b> to compare the shapes of the arrays'

        try:
            correct_shape_rlt = tuple((shape_velx[1], shape_velx[2], shape_velx[3]))
            if shape_rlt != correct_shape_rlt:
                if not self.parent.get_usersettings()['artificial_relative_layer_thickness']:
                    VALID['value'] = False
                VALID['error6'] = 'Shapes of the selected <b>Relative layer thickness</b> is not correct. Expected: {0}. Got: {1}'.format(correct_shape_rlt, shape_rlt)

        except Exception as e:
            print e
            VALID['value'] = False
            VALID['error7'] = 'Set inputs to <b>Relative layer thickness</b> and <b>Velocity X</b> to compare the shapes of the arrays'



        if not VALID['value']:
            VALID['error8'] = ' - following dimensions are expected:<br>'
            VALID['error8'] += ' '*5+'velx, vely, spm (time, z, y, x)<br>'
            VALID['error8'] += ' '*5+'water_depth (time, y, x)<br>'
            VALID['error8'] += ' '*5+'relative_layer_thickness (z, y, x)<br>'
            
            msg = 'Folowing input errors were detected:'
            for error_key in VALID.keys():
                if error_key != 'value':
                    msg = msg+'<br> - {0}'.format(VALID[error_key])
            QMessageBox.question(self, 'Warning', msg, QMessageBox.Ok)
        
        if 'error6' in VALID.keys() and VALID['value']:
            msg = '<b>Relative layer thickness</b> is not correct. Will generate some dummy-data. See parameter "artificial_relative_layer_thickness" in the user_settings file<br>'+VALID['error6']
            QMessageBox.question(self, 'Warning', msg, QMessageBox.Ok)

        return VALID['value']




    @pyqtSignature("QString")
    def on_lineEdit_i1_textChanged(self, new_str):
        #print 'lat1 textChanged >>>', str(new_str)
        try:
            new_val = int(str(new_str))
            if None not in self.data.get_p1():
                self.data.set_p1([new_val, self.data.get_p1()[1]])
            else:
                self.data.set_p1([None, new_val])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_i1.setText(QString(str(self.data.get_p1()[0])))


    @pyqtSignature("QString")
    def on_lineEdit_j1_textChanged(self, new_str):
        try:
            new_val = int(str(new_str))
            if None not in self.data.get_p1():
                self.data.set_p1([self.data.get_p1()[0], new_val])
            else:
                self.data.set_p1([new_val, None])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_j1.setText(QString(str(self.data.get_p1()[1])))

    @pyqtSignature("QString")
    def on_lineEdit_i2_textChanged(self, new_str):
        try:
            new_val = int(str(new_str))
            if None not in self.data.get_p2():
                self.data.set_p2([new_val, self.data.get_p2()[1]])
            else:
                self.data.set_p2([None, new_val])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_i2.setText(QString(str(self.data.get_p2()[0])))


    @pyqtSignature("QString")
    def on_lineEdit_j2_textChanged(self, new_str):
        try:
            new_val = int(str(new_str))
            if None not in self.data.get_p2():
                self.data.set_p2([self.data.get_p2()[0], new_val])
            else:
                self.data.set_p2([new_val, None])
        except Exception as e:
            QMessageBox.question(self, 'Error', str(e), QMessageBox.Ok)
            self.lineEdit_j2.setText(QString(str(self.data.get_p2()[1])))


    @pyqtSignature("")
    def on_pushButton_pick1_clicked(self):
        self.toogle_show_after_selected_cell = 1
        self.hide()

    def on_pushButton_pick2_clicked(self):
        self.toogle_show_after_selected_cell = 2
        self.hide()


    @pyqtSignature("")
    def on_pushButton_selectBathymetry_clicked(self):
        openpath = os.path.dirname(self.parent.filename)
        fname = unicode(QFileDialog.getOpenFileName(self, 'Open data file', openpath, filter='NetCDF files (*.nc)'))
        if fname:
            self.get_grid_info(fname)
        else:
            QMessageBox.question(self, 'Error', str('ERROR! Could not open file {1}'.format(fname)), QMessageBox.Ok)

    


    def accept(self):
        if self.validate_input():
            nc = self.parent.get_selected_ncfile_instance()
            self.update()  # saving current data

            # open file , get data whole data
                       
            p1 = self.data.get_p1()
            p2 = self.data.get_p2()
            

            lats, lons = self.data.get_latNcVar(), self.data.get_lonNcVar()

            if p1[1] == p2[1]:  #x are same
                print 'picking vely:', unicode(self.comboBox_vely.currentText())
                vel4D = nc.variables[unicode(self.comboBox_vely.currentText())][:]
            elif p1[0] == p2[0]:  # y are same
                print 'picking velx:', unicode(self.comboBox_velx.currentText())
                vel4D = nc.variables[unicode(self.comboBox_velx.currentText())][:]
            else:
                QMessageBox.question(self, 'Error', 'smthing wrong with points... cmon...', QMessageBox.Ok)

            print 'picking spm:', unicode(self.comboBox_spm.currentText())
            spm4D = nc.variables[unicode(self.comboBox_spm.currentText())][:]
            print 'picking water_depth:', unicode(self.comboBox_waterDepth.currentText())
            wd3D = nc.variables[unicode(self.comboBox_waterDepth.currentText())][:]
            
            if not self.parent.get_usersettings()['artificial_relative_layer_thickness']:
                print 'picking relative_layer_thickness:', unicode(self.comboBox_relativeLayerThickness.currentText())
                rlt3D = nc.variables[unicode(self.comboBox_relativeLayerThickness.currentText())][:]
            else:
                # -------------------------------------------------------------
                # artificial_data (rlt3D)
                shape = [vel4D.shape[1], vel4D.shape[2], vel4D.shape[3]]
                rlt3D = np.empty(shape)
                print 'creating dummy relative_layer_thickness, where every value is ', 1./float(shape[0])
                rlt3D[:] = 1./float(shape[0])
                print 'data shape = ', vel4D.shape
                print 'artificial relative_layer_thickness shape =', rlt3D.shape
                # artificial_data (rlt3D)
                # -------------------------------------------------------------
            nc.close()
            


            # -------------------------------------------------------------
            # Now cerating layerHeight array based on <Relative_layer_thickness> and <water_depth>
            print 'calculating Layer_thickness...'
            shape = list(vel4D.shape[:])
            layerh4D = np.empty(shape)
            for t in xrange(shape[0]):
                for z in xrange(shape[1]):
                    layerh4D[t, z, :, :] = np.multiply(wd3D[t, :, :], rlt3D[z, :, :])
            # finished  cerating layerHeight array based on <Relative_layer_thickness> and <water_depth>
            # -------------------------------------------------------------



            data = dict()
            data['t'] = t
            data['time'] = self.parent.get_time1D_array()
            data['p1'] = p1
            data['p2'] = p2
            data['lats'] = lats
            data['lons'] = lons
            data['layerh4D'] = layerh4D
            data['vel4D'] = vel4D
            data['spm4D'] = spm4D

            gui = tab_widget_flux(self, data, plot=True)
            self.done(1)


        else:
            pass

    def reject(self):
        self.clear_data()
        super(My_dlg, self).reject()






    def get_grid_info(self, fname):
        f = netCDF4.Dataset(fname, mode='r')
        LAT_NAME_LIST = ['lat', 'lats', 'getmGrid2D_getm_lat']
        LON_NAME_LIST = ['lon', 'lons', 'getmGrid2D_getm_lon']
        lat_var , lon_var = None, None
        d_lat, d_lon = None, None
        nc = self.parent.get_selected_ncfile_instance()
        for LAT in LAT_NAME_LIST:
            if LAT in f.variables.keys():
                lat_var = f.variables[LAT][:]
                current_nc_var = nc.variables[str(self.parent.comboBox_dataType.currentText())]
                data , _ = self.parent.detectPlotType_andReturnData(current_nc_var, detect_plot_type=False)
                if len(lat_var) != data.shape[0]:
                    QMessageBox.question(self, 'Error', str('ERROR! First dimensiond of data-array shape {0} differes from lat-array shape {1}. Must be same.'.format(data.shape, lat_var.shape)), QMessageBox.Ok)
        if lat_var is None:
            QMessageBox.question(self, 'Error', str('ERROR! None of vars specifying LAT ("{0}") not found in file {1}'.format(LAT_NAME_LIST, fname)), QMessageBox.Ok)
        else:
            d_lat = float(lat_var[1]-lat_var[0])
        for LON in LON_NAME_LIST:
            if LON in f.variables.keys():
                lon_var = f.variables[LON][:]
                data , _ = self.parent.detectPlotType_andReturnData(current_nc_var, detect_plot_type=False)
                if len(lon_var) != data.shape[1]:
                    QMessageBox.question(self, 'Error', str('ERROR! First dimensiond of data-array shape {0} differes from lon-array shape {1}. Must be same.'.format(data.shape, lon_var.shape)), QMessageBox.Ok)
        if lon_var is None:
            QMessageBox.question(self, 'Error', str('ERROR! None of vars specifying LON ("{0}") not found in file {1}'.format(LON_NAME_LIST, fname)), QMessageBox.Ok)
        else:
            d_lon = float(lon_var[1]-lon_var[0])

        if d_lat and d_lon:
            self.data.set_bath(fname)
            self.data.set_dlat_dlon(d_lat, d_lon)
            self.data.set_lonVar(LON)
            self.data.set_latVar(LAT)
            self.data.set_lonNcVar(lon_var)
            self.data.set_latNcVar(lat_var)
            self.update()
            nc.close()
            f.close()
            return (d_lat , d_lon)
        else:
            nc.close()
            f.close()
            QMessageBox.question(self, 'Error', str('ERROR! Could not find LAT, LON in {1}'.format(fname)), QMessageBox.Ok)
            return

