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
from PyQt4.QtGui import QWidget, QMessageBox, QColorDialog, QColor
import ui_gui
from mpl_toolkits.basemap import Basemap, projection_params, supported_projections
import matplotlib.pyplot as plt
import netCDF4


# use this if you want to include modules from a subfolder
import inspect, sys, os
relative_paths = [
    '..',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

from CodeGenerator import CodeGeneratorBackend
from dynamic_import import importCode


class prj_dlg(QWidget, ui_gui.Ui_Dialog):
    def __init__(self, parent=None):
        super(prj_dlg, self).__init__()
        self.parent = parent
        self.setupUi(self)
        self.DATA = dict()
        self.DATA['colors'] = dict()
        self.get_grid_info_and_data()


        self.init_tooltips()
        self.init_widget_lists()
        self.init_colorframes()
        self.init_comboBox_selectProjection()
        self.init_comboBox_cbar_loc()
        self.init_comboBox_projectionResolution()
        self.init_corners()
        self.init_scale()
        self.init_meridians_parallels()
        #self.connect()



    def init_tooltips(self):
        self.checkBox_fillOcean.setToolTip('Fill oceans, seas, and lakes. Pick color')
        self.checkBox_fillContinent.setToolTip('Fill continents. Pick color')
        self.checkBox_drawRiver.setToolTip('Draw major rivers. Pick color')
        self.checkBox_drawCoasline.setToolTip('Draw coastlines on the map')

        self.comboBox_selectProjection.setToolTip('Select projection from list')
        self.comboBox_projectionResolution.setToolTip('resolution of boundary database to use. Resolution drops off by roughly 80% between datasets. Higher res datasets are much slower to draw')
        self.lineEdit_llcrnrlon.setToolTip('longitude of lower left hand corner of the desired map domain (degrees)')
        self.lineEdit_llcrnrlat.setToolTip('latitude of lower left hand corner of the desired map domain (degrees)')
        self.lineEdit_urcrnrlat.setToolTip('longitude of upper right hand corner of the desired map domain (degrees)')
        self.lineEdit_urcrnrlon.setToolTip('latitude of upper right hand corner of the desired map domain (degrees)')

        self.lineEdit_width.setToolTip('width of desired map domain in projection coordinates (meters)')
        self.lineEdit_height.setToolTip('height of desired map domain in projection coordinates (meters)')
        self.lineEdit_lon0.setToolTip('center of desired map domain (in degrees)')
        self.lineEdit_lat0.setToolTip('center of desired map domain (in degrees)')

        self.groupBox_parallels.setToolTip('Draw and label parallels (latitude lines) for values (in degrees) given in the sequence <i>circles</i>')
        self.lineEdit_par_range.setToolTip('[start, stop, step] parallels values to draw. Example: [0, 100, 20] will return a list of (0, 20, 40, 60, 80)')
        self.pushButton_par_color.setToolTip('select color to draw parallels')
        self.frame_par_color.setToolTip('color to draw parallels')
        self.lineEdit_par_zorder.setToolTip('sets the zorder for parallels (if not specified, uses default zorder for matplotlib.lines.Line2D objects)')
        self.lineEdit_par_linewidth.setToolTip('line width for parallels (default 1.)')
        self.lineEdit_par_labels.setToolTip('list of 4 values (default [0,0,0,0]) that control whether parallels are labelled where they intersect the left, right, top or bottom of the plot.<br> For example labels=[1,0,0,1] will cause parallels to be labelled where they intersect the left and and bottom of the plot, but not the right and top')
        self.lineEdit_par_dashes.setToolTip('dash pattern for parallels (default [1,1], i.e. 1 pixel on, 1 pixel off)')
        self.lineEdit_par_fmt.setToolTip("a format string to format the parallel labels (default '%g')")

        self.groupBox_meridians.setToolTip('Draw and label meridians (longitude lines) for values (in degrees) given in the sequence <i>circles</i>')
        self.lineEdit_mer_range.setToolTip('[start, stop, step] meridians values to draw. Example: [0, 100, 20] will return a list of (0, 20, 40, 60, 80)')
        self.pushButton_mer_color.setToolTip('select color to draw meridians')
        self.frame_mer_color.setToolTip('color to draw meridians')
        self.lineEdit_mer_zorder.setToolTip('sets the zorder for meridians (if not specified, uses default zorder for matplotlib.lines.Line2D objects)')
        self.lineEdit_mer_linewidth.setToolTip('line width for meridians (default 1.)')
        self.lineEdit_mer_labels.setToolTip('list of 4 values (default [0,0,0,0]) that control whether meridians are labelled where they intersect the left, right, top or bottom of the plot.<br> For example labels=[1,0,0,1] will cause meridians to be labelled where they intersect the left and and bottom of the plot, but not the right and top')
        self.lineEdit_mer_dashes.setToolTip('dash pattern for meridians (default [1,1], i.e. 1 pixel on, 1 pixel off)')
        self.lineEdit_mer_fmt.setToolTip("a format string to format the meridians labels (default '%g')")

        self.comboBox_cbar_loc.setToolTip("where to put colorbar")
        self.lineEdit_cbar_size.setToolTip("width of colorbar axes (string 'N%', where N is an integer describing the fractional width of the parent axes). Default '5%'.")
        self.lineEdit_cbar_pad.setToolTip("Padding between parent axes and colorbar axes in same units as size. Default '2%'")
        self.lineEdit_cbar_label.setToolTip("label which will be displayed near the colorbar")
        self.checkBox_cbar_addcontourlines.setToolTip("add ticks of counter-lines values to colorbar")

        self.checkBox_pcolormesh.setToolTip("Make a pseudo-color plot over the map (see matplotlib.pyplot.pcolormesh documentation)")
        self.groupBox_contour.setToolTip("Make a contour plot over the map (see matplotlib.pyplot.contour documentation).")
        self.checkBox_contour_fill.setToolTip("Make a filled contour plot over the map (see matplotlib.pyplot.contourf documentation).")
        self.lineEdit_contour_range.setToolTip('[start, stop, step] contour values to draw. Example: [0, 100, 20] will return a list of (0, 20, 40, 60, 80)<br> If None, False or empty - chosen automatically')
        self.checkBox_contour_label.setToolTip('Label a contour plot.')
        self.checkBox_contour_label_inline.setToolTip('controls whether the underlying contour is removed or not. Default is True')
        self.lineEdit_contour_lable_fmt.setToolTip('a format string for the label. Default is "%1.3f"')
        self.lineEdit_contour_label_fntsize.setToolTip('size in points or relative size')


        self.groupBox_scale.setToolTip("Draw a map scale at <i>lon,lat</i> of length <i>length</i> representing distance in the map projection coordinates at center of the map")
        self.lineEdit_scale_fmt.setToolTip("a string formatter to format numeric values")
        self.lineEdit_scale_fontsize.setToolTip("for map scale annotations, default 9")
        self.lineEdit_scale_lat.setToolTip("Draw a map scale at <i>lon,lat</i>")
        self.lineEdit_scale_lon.setToolTip("Draw a map scale at <i>lon,lat</i>")
        self.lineEdit_scale_length.setToolTip("Draw a map scale of length <i>length</i> representing distance in the map projection coordinates")
        self.lineEdit_scale_units.setToolTip("the units of the length argument (Default km)")





    @pyqtSignature("int")
    def on_checkBox_fillOcean_stateChanged(self, state_int):
        if self.checkBox_fillOcean.isChecked():
            self.showColorDialog(self.frame_fillOceanColor)

    @pyqtSignature("int")
    def on_checkBox_fillContinent_stateChanged(self, state_int):
        if self.checkBox_fillContinent.isChecked():
            self.showColorDialog(self.frame_fillContinentColor)

    @pyqtSignature("int")
    def on_checkBox_drawRiver_stateChanged(self, state_int):
        if self.checkBox_drawRiver.isChecked():
            self.showColorDialog(self.frame_fillRiverColor)

    @pyqtSignature("int")
    def on_checkBox_pcolormesh_stateChanged(self, state_int):
        if self.checkBox_pcolormesh.isChecked():
            self.checkBox_contour_fill.setCheckState(Qt.Unchecked)

    @pyqtSignature("int")
    def on_checkBox_contour_fill_stateChanged(self, state_int):
        if self.checkBox_contour_fill.isChecked():
            self.checkBox_pcolormesh.setCheckState(Qt.Unchecked)
            self.checkBox_cbar_addcontourlines.setCheckState(Qt.Unchecked)
            self.checkBox_cbar_addcontourlines.setEnabled(False)
        else:
            self.checkBox_cbar_addcontourlines.setEnabled(True)

    @pyqtSignature("")
    def on_pushButton_mer_color_clicked(self):
        self.showColorDialog(self.frame_mer_color)

    @pyqtSignature("")
    def on_pushButton_par_color_clicked(self):
        self.showColorDialog(self.frame_par_color)

    @pyqtSignature("")
    def on_groupBox_contour_clicked(self):
        if self.groupBox_contour.isChecked():
            self.checkBox_cbar_addcontourlines.setEnabled(True)
        else:
            self.checkBox_cbar_addcontourlines.setCheckState(Qt.Unchecked)
            self.checkBox_cbar_addcontourlines.setEnabled(False)

    @pyqtSignature("int")
    def on_checkBox_contour_label_stateChanged(self, state_int):
        for w in self.contour_lables[1::]:
            w.setEnabled(self.checkBox_contour_label.isChecked())

    @pyqtSignature("")
    def on_pushButton_gen_clicked(self):
        code = self.generate_code()
        self.textEdit.clear()
        self.textEdit.setText(code)
        self.tabWidget.setCurrentWidget(self.tab_4)


    @pyqtSignature("")
    def on_pushButton_run_clicked(self):
        if unicode(self.textEdit.toPlainText()) == u'':
            self.tabWidget.setCurrentWidget(self.tab_4)
            QMessageBox.question(self, 'Error', "First generate script. Tab TextEdit is emty", QMessageBox.Ok); return
        else:
            '''
            # save script
            fname = 'plot_tmp.py'
            dirname = os.path.dirname(os.path.realpath(__file__))
            try:
                os.remove(os.path.join(dirname, fname))
                os.remove(os.path.join(dirname, fname+'c'))
            except Exception as e: pass

            print "saving file"
            f = open(os.path.join(dirname, fname), 'w')
            f.write(unicode(self.textEdit.toPlainText()))
            f.close()
            # import newly created file
            import plot_tmp ; reload(plot_tmp)
            '''

            # load dynamically the code from QTextEdit
            raw_code = unicode(self.textEdit.toPlainText())
            plot_tmp = importCode(raw_code, 'plot_tmp')

            # make figure
            f = plt.figure()
            ax = plt.subplot(111)

            # update data and plot!
            self.get_grid_info_and_data()
            plot_tmp.plot_projected_data(ax, self.DATA['lon'], self.DATA['lat'], self.DATA['data'], self.parent.cb.get_cmap())
            f.show()

    @pyqtSignature("")
    def on_pushButton_cancel_clicked(self):
        self.close()






    def init_widget_lists(self):
        self.color_frames = [self.frame_mer_color,
                             self.frame_par_color,
                             self.frame_fillContinentColor,
                             self.frame_fillOceanColor,
                             self.frame_fillRiverColor]

        self.contour_lables = [self.checkBox_contour_label,
                               self.checkBox_contour_label_inline,
                               self.lineEdit_contour_label_fntsize,
                               self.lineEdit_contour_lable_fmt]

    def init_colorframes(self):
        col = QColor(0, 0, 0)
        for frm in self.color_frames:
            frm.setStyleSheet("QWidget { background-color: %s }" % col.name())
            self.DATA['colors'][unicode(frm.objectName())] = unicode(col.name())

        col = QColor(85, 170, 255)
        for frm in [self.frame_fillOceanColor, self.frame_fillRiverColor]:
            frm.setStyleSheet("QWidget { background-color: %s }" % col.name())
            self.DATA['colors'][unicode(frm.objectName())] = unicode(col.name())

        col = QColor(170, 85, 0)
        for frm in [self.frame_fillContinentColor]:
            frm.setStyleSheet("QWidget { background-color: %s }" % col.name())
            self.DATA['colors'][unicode(frm.objectName())] = unicode(col.name())


    def init_comboBox_selectProjection(self):
        """
        for whatever reason, imported from matplotlib class variable
        supported_projections is a type of string, not dictionary as
        expected. First manually convert it to dict
        """
        self.DATA['supported_projections'] = dict()
        for prj in supported_projections.split("\n"):
            prj.strip()
            if not prj:
                continue
            prj_key, prj_name = unicode(prj.split(None, 1)[0].strip()), unicode(prj.split(None, 1)[1].strip())
            self.DATA['supported_projections'][prj_key] = prj_name

        self.comboBox_selectProjection.clear()
        for v in sorted(self.DATA['supported_projections'].values()):
            self.comboBox_selectProjection.addItem(v)

    def init_comboBox_cbar_loc(self):
        self.comboBox_cbar_loc.clear()
        for k in ['right', 'top', 'bottom', 'left']:
            self.comboBox_cbar_loc.addItem(unicode(k))

    def init_comboBox_projectionResolution(self):
        self.comboBox_projectionResolution.clear()
        for v in ['crude', 'low', 'intermidiate', 'high', 'full']:
            self.comboBox_projectionResolution.addItem(unicode(v))


    def showColorDialog(self, frame):
        col = QColorDialog.getColor()
        if col.isValid():
            frame.setStyleSheet("QWidget { background-color: %s }" % col.name())
            self.DATA['colors'][unicode(frame.objectName())] = unicode(col.name())


    def init_corners(self):
        self.lineEdit_urcrnrlon.setText(QString('{0:.3f}'.format(self.DATA['lon'].max())))
        self.lineEdit_urcrnrlat.setText(QString('{0:.3f}'.format(self.DATA['lat'].max())))
        self.lineEdit_llcrnrlon.setText(QString('{0:.3f}'.format(self.DATA['lon'].min())))
        self.lineEdit_llcrnrlat.setText(QString('{0:.3f}'.format(self.DATA['lat'].min())))
        self.lineEdit_lon0.setText(QString('{0:.3f}'.format((self.DATA['lon'].max()-self.DATA['lon'].min())/2.+self.DATA['lon'].min() )))
        self.lineEdit_lat0.setText(QString('{0:.3f}'.format((self.DATA['lat'].max()-self.DATA['lat'].min())/2.+self.DATA['lat'].min() )))

    def init_scale(self):
        self.lineEdit_scale_lat.setText(QString('{0:.3f}'.format(self.DATA['lat'].min() + (self.DATA['lat'].max()-self.DATA['lat'].min())/10.)))
        self.lineEdit_scale_lon.setText(QString('{0:.3f}'.format(self.DATA['lon'].min() + (self.DATA['lon'].max()-self.DATA['lon'].min())/10.)))

    def init_meridians_parallels(self):
        self.lineEdit_mer_range.clear()
        self.lineEdit_par_range.clear()
        self.lineEdit_par_range.setText(QString('{0}, {1}, 1'.format(int(self.DATA['lat'].min()-.5), int(self.DATA['lat'].max()+.5) )))
        self.lineEdit_mer_range.setText(QString('{0}, {1}, 1'.format(int(self.DATA['lon'].min()-.5), int(self.DATA['lon'].max()+.5) )))






    # -----------------------------------------------------------------------------------------
    # -----------------------------------------------------------------------------------------
    # -----------------------------------------------------------------------------------------
    # ----------------------------- GETTERS !!!!!! --------------------------------------------
    # -----------------------------------------------------------------------------------------
    # -----------------------------------------------------------------------------------------
    # -----------------------------------------------------------------------------------------


    def get_selectedProjectionShortName(self):
        current_fullname = unicode(str(self.comboBox_selectProjection.currentText()))
        for k, v in self.DATA['supported_projections'].iteritems():
            if v == current_fullname:
                return k

    def get_selectedProjectionResolution(self):
        res = unicode(str(self.comboBox_projectionResolution.currentText()))
        d = dict()
        d[u"crude"] = u'c'
        d[u"low"] = u'l'
        d[u"intermidiate"] = u'i'
        d[u"high"] = u'h'
        d[u"full"] = u'f'
        return d[res]

    def get_projectionCorners(self):
        try: llcrnrlon = float(str(self.lineEdit_llcrnrlon.text()))
        except: QMessageBox.question(self, 'Error', "Cannot convert llcrnrlon to float", QMessageBox.Ok); llcrnrlon = None
        try: llcrnrlat = float(str(self.lineEdit_llcrnrlat.text()))
        except: QMessageBox.question(self, 'Error', "Cannot convert llcrnrlat to float", QMessageBox.Ok); llcrnrlat = None
        try: urcrnrlon = float(str(self.lineEdit_urcrnrlon.text()))
        except: QMessageBox.question(self, 'Error', "Cannot convert urcrnrlon to float", QMessageBox.Ok); urcrnrlon = None
        try: urcrnrlat = float(str(self.lineEdit_urcrnrlat.text()))
        except: QMessageBox.question(self, 'Error', "Cannot convert urcrnrlat to float", QMessageBox.Ok); urcrnrlat = None

        return (llcrnrlon, llcrnrlat, urcrnrlon, urcrnrlat)


    def get_projectionLon0Lat0HeightWidth(self):
        try: lon0 = float(str(self.lineEdit_lon0.text()))
        except: QMessageBox.question(self, 'Error', "Cannot convert lon0 to float", QMessageBox.Ok); lon0 = None
        try: lat0 = float(str(self.lineEdit_lat0.text()))
        except: QMessageBox.question(self, 'Error', "Cannot convert lat0 to float", QMessageBox.Ok); lat0 = None
        try: height = float(str(self.lineEdit_height.text()))
        except: height = None  #QMessageBox.question(self, 'Error', "Cannot convert height to float", QMessageBox.Ok); height = None
        try: width = float(str(self.lineEdit_width.text()))
        except: width = None  #QMessageBox.question(self, 'Error', "Cannot convert width to float", QMessageBox.Ok); width = None

        return (lon0, lat0, height, width)


    def get_meridiansSettings(self):
        data = dict()
        data["draw"] = self.groupBox_meridians.isChecked()
        if data["draw"]:
            data["color"] = self.DATA["colors"]["frame_mer_color"]

            try: l = [float(v) for v in str(self.lineEdit_mer_range.text()).split(",")[0:3]]
            except: QMessageBox.question(self, 'Error', "Cannot convert meridian-circles linEdit content to three floats", QMessageBox.Ok); return None
            data["circles"] = l

            try: v = float(str(self.lineEdit_mer_linewidth.text()))
            except: QMessageBox.question(self, 'Error', "Cannot convert meridian-linewidth linEdit content to float", QMessageBox.Ok); return None
            data["linewidth"] = v

            try: l = [int(v) for v in str(self.lineEdit_mer_dashes.text()).split(",")[0:2]]
            except: QMessageBox.question(self, 'Error', "Cannot convert meridian-dashes linEdit content to two integers", QMessageBox.Ok); return None
            data["dashes"] = l

            try: v = int(str(self.lineEdit_mer_zorder.text()))
            except:
                if str(self.lineEdit_mer_zorder.text()) == 'None': v = None
                else: QMessageBox.question(self, 'Error', "Cannot convert meridian-zorder linEdit content to integer or None", QMessageBox.Ok); return None
            data["zorder"] = v

            try: l = [int(v) for v in str(self.lineEdit_mer_labels.text()).split(",")[0:4]]
            except: QMessageBox.question(self, 'Error', "Cannot convert meridian-label linEdit content to four integers", QMessageBox.Ok); return None
            data["labels"] = l

            try: s = unicode(self.lineEdit_mer_fmt.text()).strip()
            except: QMessageBox.question(self, 'Error', "Cannot convert meridian-format linEdit content to string", QMessageBox.Ok); return None
            data["fmt"] = s
        return data

    def get_parallelsSettings(self):
        data = dict()
        data["draw"] = self.groupBox_parallels.isChecked()
        if data["draw"]:
            data["color"] = self.DATA["colors"]["frame_par_color"]

            try: l = [float(v) for v in str(self.lineEdit_par_range.text()).split(",")[0:3]]
            except: QMessageBox.question(self, 'Error', "Cannot convert parallels-circles linEdit content to three floats", QMessageBox.Ok); return None
            data["circles"] = l

            try: v = float(str(self.lineEdit_par_linewidth.text()))
            except: QMessageBox.question(self, 'Error', "Cannot convert parallels-linewidth linEdit content to float", QMessageBox.Ok); return None
            data["linewidth"] = v

            try: l = [int(v) for v in str(self.lineEdit_par_dashes.text()).split(",")[0:2]]
            except: QMessageBox.question(self, 'Error', "Cannot convert parallels-dashes linEdit content to two integers", QMessageBox.Ok); return None
            data["dashes"] = l

            try: v = int(str(self.lineEdit_par_zorder.text()))
            except:
                if str(self.lineEdit_par_zorder.text()) == 'None': v = None
                else: QMessageBox.question(self, 'Error', "Cannot convert parallels-zorder linEdit content to integer or None", QMessageBox.Ok); return None
            data["zorder"] = v

            try: l = [int(v) for v in str(self.lineEdit_par_labels.text()).split(",")[0:4]]
            except: QMessageBox.question(self, 'Error', "Cannot convert parallels-label linEdit content to four integers", QMessageBox.Ok); return None
            data["labels"] = l

            try: s = unicode(self.lineEdit_par_fmt.text()).strip()
            except: QMessageBox.question(self, 'Error', "Cannot convert parallels-format linEdit content to string", QMessageBox.Ok); return None
            data["fmt"] = s
        return data

    def get_contourSettings(self):
        data_1 = dict()
        data_1["draw"]  = self.groupBox_contour.isChecked()
        if data_1["draw"]:
            data_1["fill"] = self.checkBox_contour_fill.isChecked()

            try: l = [float(v) for v in str(self.lineEdit_contour_range.text()).split(",")[0:3]]
            except: l = None
            data_1["range"] = l

        data_2 = dict()
        data_2["draw"]  = self.checkBox_contour_label.isChecked()
        if data_2["draw"]:
            data_2["inline"] = self.checkBox_contour_label_inline.isChecked()

            try: s = unicode(self.lineEdit_contour_lable_fmt.text()).strip()
            except: QMessageBox.question(self, 'Error', "Cannot convert contour-format linEdit content to string", QMessageBox.Ok); return None
            data_2["fmt"] = s

            try: v = float(str(self.lineEdit_contour_label_fntsize.text()))
            except: QMessageBox.question(self, 'Error', "Cannot convert contour-fontsize linEdit content to float", QMessageBox.Ok); return None
            data_2["fontsize"] = v

        return data_1, data_2


    def get_colorbarSettings(self):
        data = dict()
        data["draw"]  = self.groupBox_cbar.isChecked()
        if data["draw"]:
            data["location"] = unicode(self.comboBox_cbar_loc.currentText())

            try: s = unicode(self.lineEdit_cbar_pad.text()).strip()
            except: s = u"2%"
            data["pad"] = s

            try: s = unicode(self.lineEdit_cbar_size.text()).strip()
            except: s = u"5%"
            data["pad"] = s

            try: s = unicode(self.lineEdit_cbar_label.text()).strip()
            except: s = None
            data["label"] = s

            if self.checkBox_cbar_addcontourlines.isEnabled():
                data["cbar_addcontourlines"] = self.checkBox_cbar_addcontourlines.isChecked()
            else:
                data["cbar_addcontourlines"] = False
        return data

    def get_scaleSettings(self):
        data = dict()
        data["draw"]  = self.groupBox_scale.isChecked()
        if data["draw"]:

            try: v = float(str(self.lineEdit_scale_lon.text()))
            except: return None
            data["lon"] = v

            try: v = float(str(self.lineEdit_scale_lat.text()))
            except: return None
            data["lat"] = v

            try: v = float(str(self.lineEdit_scale_length.text()))
            except: v = 10000.
            data["length"] = v

            try: s = unicode(self.lineEdit_scale_units.text()).strip()
            except: s = u'km'
            data["units"] = s

            try: v = float(str(self.lineEdit_scale_fontsize.text()))
            except: v = 9.
            data["fontsize"] = v

            try: s = unicode(self.lineEdit_scale_fmt.text()).strip()
            except: s = u'%d'
            data["format"] = s


            data["lon0"] = (self.DATA['lon'].max()-self.DATA['lon'].min())/2.+self.DATA['lon'].min()
            data["lat0"] = (self.DATA['lat'].max()-self.DATA['lat'].min())/2.+self.DATA['lat'].min()
        return data




















    def save_current_settings(self):
        # flush existing settings-data
        try: del self.DATA["current"]
        except: pass
        self.DATA["current"] = dict()



        self.DATA["current"]["prj"] = dict()
        self.DATA["current"]["prj"]['projection'] = self.get_selectedProjectionShortName()
        self.DATA["current"]["prj"]['resolution'] = self.get_selectedProjectionResolution()  # coarse/low/medium/high/full

        corners = self.get_projectionCorners()
        self.DATA["current"]["prj"]['llcrnrlon'] = corners[0]
        self.DATA["current"]["prj"]['llcrnrlat'] = corners[1]
        self.DATA["current"]["prj"]['urcrnrlon'] = corners[2]
        self.DATA["current"]["prj"]['urcrnrlat'] = corners[3]
        lon0, lat0, height, width = self.get_projectionLon0Lat0HeightWidth()
        self.DATA["current"]["prj"]['lon_0'] = lon0
        self.DATA["current"]["prj"]['lat_0'] = lat0
        self.DATA["current"]["prj"]['height'] = height
        self.DATA["current"]["prj"]['width'] = width

        # ---------------------------------
        # map background
        # ---------------------------------
        self.DATA["current"]["drawCoasline"] = dict()
        self.DATA["current"]["drawCoasline"]["draw"] = self.checkBox_drawCoasline.isChecked()

        self.DATA["current"]["fillContinent"] = dict()
        self.DATA["current"]["fillContinent"]["color"] = self.DATA["colors"]["frame_fillContinentColor"]
        self.DATA["current"]["fillContinent"]["draw"] = self.checkBox_fillContinent.isChecked()

        self.DATA["current"]["fillOcean"] = dict()
        self.DATA["current"]["fillOcean"]["color"] = self.DATA["colors"]["frame_fillOceanColor"]
        self.DATA["current"]["fillOcean"]["draw"] = self.checkBox_fillOcean.isChecked()

        self.DATA["current"]["drawRiver"] = dict()
        self.DATA["current"]["drawRiver"]["color"] = self.DATA["colors"]["frame_fillRiverColor"]
        self.DATA["current"]["drawRiver"]["draw"] = self.checkBox_drawRiver.isChecked()
        # ---------------------------------
        #  END map background END
        # ---------------------------------
        # ---------------------------------
        # parallels, meridians
        # ---------------------------------
        self.DATA["current"]["meridians"] = self.get_meridiansSettings()
        self.DATA["current"]["parallels"] = self.get_parallelsSettings()

        # ---------------------------------
        # data
        # ---------------------------------
        self.DATA["current"]["pcolormesh"] = dict()
        self.DATA["current"]["pcolormesh"]["draw"] = self.checkBox_pcolormesh.isChecked()
        self.DATA["current"]["contour"], self.DATA["current"]["clabel"] = self.get_contourSettings()
        # ---------------------------------
        # colorbar
        # ---------------------------------
        self.DATA["current"]["cbar"] = self.get_colorbarSettings()

        # ---------------------------------
        # map scale
        # ---------------------------------
        self.DATA["current"]["scale"] = self.get_scaleSettings()




    def generate_code(self):
        self.save_current_settings()

        c = CodeGeneratorBackend()
        c.begin(tab="    ")
        #c.writeln('# -*- coding: utf-8 -*-')
        c.writeln('# ------------------------------------------')
        c.writeln('#                INFORMATION')
        c.writeln('# ------------------------------------------')
        c.writeln('# This script is generated automatically')
        c.writeln('# ')
        c.writeln('# When you click button Run Script, the function below will be executed')
        c.writeln('# with the inputs gathered from current data-selection in main-widget.')
        c.writeln('# ')
        c.writeln('# You can now freely modify this script before executing!')
        c.writeln('# ')
        c.writeln('# Consider exploring following two links to get an understanding of')
        c.writeln('# what does the function below exactly do:')
        c.writeln('#       https://basemaptutorial.readthedocs.org/en/latest/')
        c.writeln('#       http://matplotlib.org/basemap/')
        c.writeln('# ')
        c.writeln('# ')
        c.writeln('# ')
        c.writeln('# ------------------------------------------')
        c.writeln('#                   CODE')
        c.writeln('# ------------------------------------------')
        c.writeln('from mpl_toolkits.basemap import Basemap')
        c.writeln('import matplotlib.pyplot as plt')
        c.writeln('import numpy as np')
        c.writeln('')
        c.writeln('')
        c.writeln('def plot_projected_data(ax, lons_1d, lats_1d, data2d, cmap):')
        c.indent()
        c.writeln('""" function creates projection, plots data.')
        c.writeln('   ax      - matplotlib axes instance to plot data')
        c.writeln('   lons_1d - 1d array of longitudes of data')
        c.writeln('   lats_1d - 1d array of latitudes of data')
        c.writeln('   data2d  - 2d array with data, must have dimensions (lats_1d, lons_1d)')
        c.writeln('   cmap    - colormap"""')
        c.writeln('')
        c.writeln('')
        c.writeln('')
        c.writeln('# create 2d arrays of lat/lon')
        c.writeln('lons, lats = np.meshgrid(lons_1d, lats_1d)')
        c.writeln('')

        c.writeln('# creating projection')
        c.writeln('map = Basemap(')
        n_attrs = len(self.DATA["current"]["prj"])
        i = 0
        for k, v in self.DATA["current"]["prj"].iteritems():
            i += 1
            if i != n_attrs:
                c.writeln(unicode('              '+k+'='+repr(v)+','))
            else:
                c.writeln(unicode('              '+k+'='+repr(v)+')'))

        c.writeln('')
        c.writeln('# drawing background')
        if self.DATA["current"]["drawCoasline"]["draw"]:
            c.writeln('map.drawcoastlines(color=(0.3, 0.3, 0.3), linewidth=0.5, ax=ax)')
        if self.DATA["current"]["fillOcean"]["draw"]:
            c.writeln('map.drawmapboundary(fill_color="{0}", ax=ax)'.format(self.DATA["current"]["fillOcean"]["color"]))
        if self.DATA["current"]["fillContinent"]["draw"]:
            c.writeln('map.fillcontinents(color="{0}", lake_color="{1}", ax=ax)'.format(self.DATA["current"]["fillContinent"]["color"], self.DATA["current"]["fillOcean"]["color"]))
        if self.DATA["current"]["drawRiver"]["draw"]:
            c.writeln('map.drawrivers(color="{0}", ax=ax)'.format(self.DATA["current"]["drawRiver"]["color"]))
        c.writeln('')

        c.writeln('# parallels/ meridians')
        if self.DATA["current"]["meridians"]["draw"]:
            mers = self.DATA["current"]["meridians"]["circles"]
            arg_str = u'np.arange({0}, {1}, {2}), ax=ax'.format(mers[0], mers[1], mers[2])
            for k, v in self.DATA["current"]["meridians"].iteritems():
                if k not in ["draw", "circles"]:
                    arg_str += ', {0}={1}'.format(k, repr(v))
            c.writeln('map.drawmeridians({0})'.format(arg_str))

        if self.DATA["current"]["parallels"]["draw"]:
            mers = self.DATA["current"]["parallels"]["circles"]
            arg_str = u'np.arange({0}, {1}, {2}), ax=ax'.format(mers[0], mers[1], mers[2])
            for k, v in self.DATA["current"]["parallels"].iteritems():
                if k not in ["draw", "circles"]:
                    arg_str += ', {0}={1}'.format(k, repr(v))
            c.writeln('map.drawparallels({0})'.format(arg_str))
        c.writeln('')
        c.writeln('# get cartesian projection of coordinates')
        c.writeln('xx, yy = map(lons, lats)')
        c.writeln('')
        c.writeln('# plot data')
        if self.DATA["current"]["pcolormesh"]["draw"]:
            c.writeln('# plot data2d as pcolormesh')
            c.writeln('colormesh = map.pcolormesh(xx, yy, data2d, cmap=cmap, vmin=data2d.min(), vmax=data2d.max(), ax=ax)')
        if self.DATA["current"]["contour"]["draw"]:
            if self.DATA["current"]["contour"]["fill"]:
                c.writeln('# plot data2d as filled contours')
                if self.DATA["current"]["contour"]["range"]:
                    l = self.DATA["current"]["contour"]["range"]
                    c.writeln('contour = map.contourf(xx, yy, data2d, np.arange({0}, {1}, {2}), cmap=cmap, ax=ax)'.
                              format(l[0], l[1], l[2]) )
                else:
                    c.writeln('contour = map.contourf(xx, yy, data2d, cmap=cmap ax=ax)')
            else:
                c.writeln('# plot data2d as contour-lines')
                if self.DATA["current"]["contour"]["range"]:
                    l = self.DATA["current"]["contour"]["range"]
                    c.writeln('contour = map.contour(xx, yy, data2d, np.arange({0}, {1}, {2}), cmap=cmap, ax=ax)'.
                              format(l[0], l[1], l[2]) )
                else:
                    c.writeln('contour = map.contour(xx, yy, data2d, cmap=cmap, ax=ax)')

            if self.DATA["current"]["clabel"]["draw"]:
                arg_str = u'contour'
                for k, v in self.DATA["current"]["clabel"].iteritems():
                    if k not in ["draw"]:
                        arg_str += u', {0}={1}'.format(k, repr(v))
                c.writeln('# label contour-lines')
                c.writeln(u'plt.clabel('+arg_str+u", colors=u'k')")

        c.writeln('')
        if self.DATA["current"]["cbar"]["draw"]:
            if self.DATA["current"]["pcolormesh"]["draw"]:
                arg_str = u'colormesh'
            elif self.DATA["current"]["contour"]["draw"]:
                arg_str = u'contour'
            else:
                QMessageBox.question(self, 'Error', "Select pcolormesh or counter plot-type", QMessageBox.Ok); return None
            for k, v in self.DATA["current"]["cbar"].iteritems():
                if k not in ["draw", "cbar_addcontourlines"]:
                    arg_str += u', {0}={1}'.format(k, repr(v))
            c.writeln('# add colorbar')
            c.writeln(u'cb = map.colorbar('+arg_str+u')')
            if self.DATA["current"]["cbar"]["cbar_addcontourlines"]:
                c.writeln(u'cb.add_lines(contour)')

        c.writeln('')
        if self.DATA["current"]["scale"]["draw"]:
            c.writeln('# add mapscale')

            lon = self.DATA["current"]["scale"]["lon"]
            lat = self.DATA["current"]["scale"]["lat"]
            lon0 = self.DATA["current"]["scale"]["lon0"]
            lat0 = self.DATA["current"]["scale"]["lat0"]
            length = self.DATA["current"]["scale"]["length"]
            arg_str = u'{0}, {1}, {2}, {3}, {4}'.format(lon, lat, lon0, lat0, length)
            for k, v in self.DATA["current"]["scale"].iteritems():
                if k not in ["draw", "lat", "lon", "lat0", "lon0", "length"]:
                    arg_str += u', {0}={1}'.format(k, repr(v))

            c.writeln(u'map.drawmapscale('+arg_str+u')')


        return c.end()






























    def get_grid_info_and_data(self, fname=None):
        if not fname:
            fname = self.parent.get_current_nc_fname()
        f = netCDF4.Dataset(fname, mode='r')
        LAT_NAME_LIST = ['lat', 'lats', 'getmGrid2D_getm_lat']
        LON_NAME_LIST = ['lon', 'lons', 'getmGrid2D_getm_lon']
        lat_var , lon_var = None, None
        nc = self.parent.get_selected_ncfile_instance()
        current_nc_var = nc.variables[self.parent.get_current_var_name()]
        data , _ = self.parent.detectPlotType_andReturnData(current_nc_var, detect_plot_type=False)

        for LAT in LAT_NAME_LIST:
            if LAT in f.variables.keys():
                lat_var = f.variables[LAT][:]
                if len(lat_var) != data.shape[0]:
                    QMessageBox.question(self, 'Error', str('ERROR! First dimensiond of data-array shape {0} differes from lat-array shape {1}. Must be same.'.format(data.shape, lat_var.shape)), QMessageBox.Ok)
        if lat_var is None:
            QMessageBox.question(self, 'Error', str('ERROR! None of vars specifying LAT ("{0}") not found in file {1}'.format(LAT_NAME_LIST, fname)), QMessageBox.Ok)

        for LON in LON_NAME_LIST:
            if LON in f.variables.keys():
                lon_var = f.variables[LON][:]
                if len(lon_var) != data.shape[1]:
                    QMessageBox.question(self, 'Error', str('ERROR! First dimensiond of data-array shape {0} differes from lon-array shape {1}. Must be same.'.format(data.shape, lon_var.shape)), QMessageBox.Ok)
        if lon_var is None:
            QMessageBox.question(self, 'Error', str('ERROR! None of vars specifying LON ("{0}") not found in file {1}'.format(LON_NAME_LIST, fname)), QMessageBox.Ok)


        if lon_var is not None and lat_var is not None:

            self.DATA['lon_varname'] = LON
            self.DATA['lat_varname'] = LAT
            self.DATA['lon'] = lon_var
            self.DATA['lat'] = lat_var
            self.DATA['data'] = data
            nc.close()
            f.close()
            return (LON, LAT, lon_var, lat_var, data)

        else:
            QMessageBox.question(self, 'Error', str('ERROR! Could not find LAT, LON in {1}'.format(fname)), QMessageBox.Ok)
            nc.close()
            f.close()
            return
