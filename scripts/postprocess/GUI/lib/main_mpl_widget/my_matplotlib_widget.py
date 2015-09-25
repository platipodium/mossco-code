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

import sys
from matplotlib.figure import Figure
from matplotlib.backends.backend_qt4agg import (
    FigureCanvasQTAgg as FigureCanvas,
    NavigationToolbar2QT as NavigationToolbar)
#from matplotlib.backends import qt_compat
import numpy as np

from PyQt4.QtCore import Qt
from PyQt4.QtGui import QPushButton, QCheckBox, QVBoxLayout, QHBoxLayout, QApplication, QWidget


import inspect, os, sys
relative_paths = [

    '..',
    '../rmb_menu',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)


from mpl_context_menu import mpl_menu


class myNavigationToolbar(NavigationToolbar):
    # only display the buttons we need
    toolitems = [t for t in NavigationToolbar.toolitems if
                 t[0] in ('Home', 'Pan', 'Zoom')]
    
    def pan(self):
        NavigationToolbar.pan(self)
        self.mode = ""  #<--- whatever you want to replace "pan/zoom" goes here
        self.set_message(self.mode)

    def zoom(self):
        NavigationToolbar.zoom(self)
        self.mode = ""  #<--- whatever you want to replace "zoom rect" goes here
        self.set_message(self.mode)


class mpl_widget(QWidget):
    def __init__(self, parent=None, mainWidget=None):
        self._SELECTEDCELLS = list()       # container for instances of selected cells, so we can delete them when we want
        self._SELECTEDCELLS_IJ = list()  # container for coords of selected cells, so we can delete them when we want
        self._SELECTEDCELLLINES = list()   # container for instances of selected cells, so we can delete them when we want
        self._GRIDLINES = None
        QWidget.__init__(self, parent)
        self.mainWidget = mainWidget
        self.create_main_frame()
        self.mpl_menu = mpl_menu(self)
        self.shift_is_held = False
        #self.connect(self.mpl_menu, QtCore.SIGNAL('mySignal'), self.mySlot)
        #print 'my parent is:', parent
        self.clear_selection()
        self.init_tooltips()

    def init_tooltips(self):
        self.canvas.setToolTip('If 2D plot => RMB click toggles menu <br> - RMB click selects cell <br> - selected cells are drawn with black border')
        self.grid_cb.setToolTip('If 2D plot => show computational grid <br> If 1D plot => show normal gridlines')
        self.cbar_button.setToolTip('If 2D plot => controls the color range. <br> Note: <br> - pressing UP and DOWN arrows cycles through colormaps <br> - dragging colorbar with RMB scales the color-range <br> - dragging colorbar with LMB shifts the color-range')
        self.mpl_toolbar.setToolTip('Shows coordinates (i,j, lat,lon) and data-value(z) under the cursor. <br> if you see <i>>>></i> coordinates are not visible. Enlarge the window')

    def create_main_frame(self):

        self.fig = Figure(dpi=100)
        self.canvas = FigureCanvas(self.fig)
        self.canvas.setParent(self)
        self.canvas.setFocusPolicy( Qt.ClickFocus )
        self.canvas.setFocus()

        self.mpl_toolbar = myNavigationToolbar(self.canvas, self)
        self.canvas.mpl_connect('button_press_event', self.on_click)
        self.canvas.mpl_connect('key_press_event', self.on_key_press)
        self.canvas.mpl_connect('key_release_event', self.on_key_release)
        #self.canvas.mpl_connect('button_press_event', self.disable_clicks)


        self.cbar_button = QPushButton("Color Range")
        self.cbar_button.setFocusPolicy( Qt.NoFocus )
        self.grid_cb = QCheckBox("Show Grid")
        self.grid_cb.setFocusPolicy( Qt.NoFocus )
        self.grid_cb.stateChanged.connect(self.showGrid)

        vbox = QVBoxLayout()
        hbox = QHBoxLayout()

        vbox.addWidget(self.canvas)  # the matplotlib canvas
        hbox.addWidget(self.mpl_toolbar)
        hbox.addWidget(self.cbar_button)
        hbox.addWidget(self.grid_cb)
        vbox.addLayout(hbox)
        self.setLayout(vbox)



    def on_click(self, event):
        if event.inaxes != self.get_axes()[0]: return
        #if self.get_axes()[0].format_coord(event.x, event.y) == 'outside data area': return
        if self.allow_menu():
            self.allow_popup_menu = True
            if self.shift_is_held:
                self.allow_popup_menu = False

            point = [int(event.xdata + .5), int(event.ydata + .5)]
            #print '>>>', point, '\t currently {0} selected'.format(len(self._SELECTEDCELLS))
            if event.button == 3 :  #if RMB is clicked

                # working with dialog for transect!
                if self.mainWidget.transect_dlg:
                    if self.mainWidget.transect_dlg.toogle_show_after_selected_cell:
                        realx, realy = self.get_real_xy(event.xdata, event.ydata, self.mainWidget.detect_variable_dimensions())
                        realpoint = [realy, realx]
                        #print 'real xy:', realpoint
                        if self.mainWidget.transect_dlg.toogle_show_after_selected_cell == 1:  # select point 1
                            self.allow_popup_menu = False
                            self.mainWidget.transect_dlg.data.set_p1(realpoint)
                        elif self.mainWidget.transect_dlg.toogle_show_after_selected_cell == 2:  # select point 2
                            self.allow_popup_menu = False
                            self.mainWidget.transect_dlg.data.set_p2(realpoint)

                        self.mainWidget.transect_dlg.update()
                        self.mainWidget.transect_dlg.show()
                
                # working with dialog for flux!
                if self.mainWidget.flux_dlg:
                    if self.mainWidget.flux_dlg.toogle_show_after_selected_cell:
                        if self.mainWidget.flux_dlg.toogle_show_after_selected_cell == 1:  # select point 1
                            self.allow_popup_menu = False
                            self.mainWidget.flux_dlg.data.set_p1(point)
                        elif self.mainWidget.flux_dlg.toogle_show_after_selected_cell == 2:  # select point 2
                            self.allow_popup_menu = False
                            self.mainWidget.flux_dlg.data.set_p2(point)

                        self.mainWidget.flux_dlg.update()
                        self.mainWidget.flux_dlg.show()



                if len(self._SELECTEDCELLS) == 0:  # if no cell is selected
                    self.add_selected_cell(point)


                else:  # if some cells are already selected
                    if self.mpl_menu.allow_rmb_select_cells() or self.shift_is_held:
                        # check if this point is already selected:
                        already_selected = False
                        for p in self._SELECTEDCELLS_IJ:
                            if (point[0] == p[0]) and (point[1] == p[1]):
                                already_selected = True
                                print 'cell already selected... is not added'

                        if not already_selected:
                            self.add_selected_cell(point)
                    else:
                        pass
                        #self.clear_selection()
                        #self.add_selected_cell(point)

    def cells_selected(self):
        if self._SELECTEDCELLS: return len(self._SELECTEDCELLS)
        else: return False

    def add_selected_cell(self, point):
        ''' point = [i, j]'''
        print 'selected cell:', point[0], point[1]
        c = self.draw_picked_cell(point)
        self._SELECTEDCELLS.append(c)
        self._SELECTEDCELLS_IJ.append(point)




    def get_selected_cells_ij(self):
        return self._SELECTEDCELLS_IJ

    def clear_selection(self):
        '''
        delete all graphical objects of selected cells
        redraw canvas
        '''
        print 'clearing stuff'
        if len(self._SELECTEDCELLLINES) > 0:
            for line in self._SELECTEDCELLLINES:
                l = line.pop(0)
                l.remove()
                del l
            del self._SELECTEDCELLLINES[:]
        #print 'cells ---- before:', len(self._SELECTEDCELLS)
        if len(self._SELECTEDCELLS) > 0:
            for cell in self._SELECTEDCELLS:
                for line in cell:
                    l = line.pop(0)
                    l.remove()
                    del l
            del self._SELECTEDCELLS[:]
        #print 'cells ---- left:', len(self._SELECTEDCELLS)


        #print 'cells-coords ----'
        #print len(self._SELECTEDCELLS_IJ)
        if self._SELECTEDCELLS_IJ:
            for cellcoords in self._SELECTEDCELLS_IJ:
                #cc = cellcoords.pop(0)
                #cellcoords.remove()
                del cellcoords
            del self._SELECTEDCELLS_IJ[:]
        #print 'cells ---- left,', len(self._SELECTEDCELLS_IJ)



        if len(self._SELECTEDCELLS) != 0:
            raise ValueError('List "self._SELECTEDCELLS" was not flushed')

        if len(self._SELECTEDCELLLINES) != 0:
            raise ValueError('List "self._SELECTEDCELLLINES" was not flushed')

        if len(self._SELECTEDCELLS_IJ) != 0:
            raise ValueError('List "self._SELECTEDCELLLINES" was not flushed')
        # update plot
        self.canvas.draw()
        #print 'finishing clear: cells left', len(self._SELECTEDCELLS)


    def showGrid(self, state):
        if self.fig.axes:
            current_plot = self.mainWidget.get_plotType()
            current_plot2D = self.mainWidget.get_plotType_for_timeseries()
            if state == Qt.Checked:
                if current_plot == '1D' or (current_plot2D =="2DZT"):
                    self.fig.axes[0].grid(True)
                elif current_plot == '2D' and (not current_plot2D =="2DZT"):
                    self.draw_pixel_grid(True)
            else:
                if current_plot == '1D' or (current_plot2D =="2DZT"):
                    self.fig.axes[0].grid(False)
                elif current_plot == '2D' and (not current_plot2D =="2DZT"):
                    self.draw_pixel_grid(False)
            self.canvas.draw()

    def draw_picked_cell(self, point):
        x = point[0]
        y = point[1]
        '''
        approach drawing a patch... not working
        cell_bnd = patches.Rectangle((x-.5, y-.5), 1, 1, fill=False, edgecolor="black", hatch=None, linewidth=1.)
        cell_instance = self.fig.axes[0].add_patch(cell_bnd)
        '''

        b_line = [(x-.5, x+.5), (y-.5, y-.5)]
        r_line = [(x+.5, x+.5), (y-.5, y+.5)]
        t_line = [(x-.5, x+.5), (y+.5, y+.5)]
        l_line = [(x-.5, x-.5), (y-.5, y+.5)]
        cell = [b_line, r_line, t_line, l_line]
        for i, l in enumerate(cell):
            ll = self.fig.axes[0].plot(l[0], l[1], 'k-', lw=.8)
            cell[i] = ll  # overwriting current Line2D object with object binded to an axes
        #self._SELECTEDCELLS.append(cell)  # collecting reference to this cell to be able to delete it
        #self._SELECTEDCELLS_IJ.append(point)  # collecting reference to this cell to be able to delete it

        self.canvas.draw()
        return cell


    def draw_line(self, point1, point2):
        line = [(point1[0], point2[0]), (point1[1], point2[1])]
        l = self.fig.axes[0].plot(line[0], line[1], 'k-', lw=2)
        return l

    def draw_pixel_grid(self, enable=True):
        if enable:
            dx = 1
            dy = 1
            x0 = -.5
            y0 = -.5

            if self.mainWidget.get_plotType_for_timeseries() == '2DXY':
                nx = self.mainWidget.get_nX()
                ny = self.mainWidget.get_nY()
            elif self.mainWidget.get_plotType_for_timeseries() == '2DZY':
                nx = self.mainWidget.get_nY()
                ny = self.mainWidget.get_nZ()
            elif self.mainWidget.get_plotType_for_timeseries() == '2DZX':
                nx = self.mainWidget.get_nX()
                ny = self.mainWidget.get_nZ()

            self._GRIDLINES = list()
            for n_hline in np.arange(ny+1):
                hline = [(x0, x0+nx), (y0+n_hline, y0+n_hline)]
                l = self.fig.axes[0].plot(hline[0], hline[1], 'k-', lw=.2)
                self._GRIDLINES.append(l)  # collecting reference to this line to be able to delete it

            for n_vline in np.arange(nx+1):
                vline = [(x0+n_vline, x0+n_vline), (y0, y0+ny)]
                l = self.fig.axes[0].plot(vline[0], vline[1], 'k-', lw=.2)
                self._GRIDLINES.append(l)  # collecting reference to this line to be able to delete it


        if not enable:
            #print 'deleting lines...'
            if self._GRIDLINES:  # if lines were created
                #print 'lines are here...'
                for line in self._GRIDLINES:
                    #print line
                    l = line.pop(0)
                    l.remove()
                    del l
            self.fig.canvas.draw()

    def on_key_press(self, event):
        #print 'key pressed:', event.key
        if event.key == 'shift':
            self.shift_is_held = True


    

    def on_key_release(self, event):
        #print 'key released:', event.key
        if event.key == 'shift':
            self.shift_is_held = False

        elif event.key == 'escape':
            self.clear_selection()


    def change_coordinate_formatter(self, ax, data2d,  bruteforce_flag=None, bruteforce_dims=None):
        ''' see http://stackoverflow.com/questions/14754931/matplotlib-values-under-cursor
        '''
        numrows, numcols = data2d.shape
        bruteforce_mode_on = False
        bruteforce_mode_on = (bruteforce_flag == '2DXY' and bruteforce_dims[-1] and bruteforce_dims[-2])


        def format_coord(x, y):
            col = int(x+0.5)
            row = int(y+0.5)

            if not bruteforce_mode_on:
                if col >= 0 and col < numcols and row >= 0 and row < numrows:
                    #z = data2d[row, col]
                    # sice we have artificially reversed y-coordinate axes, now reverse data!
                    # numrows-1, because if nrows=10 => row=[0:9]
                    z = data2d[numrows-1-row, col]
                    #return 'x=%1.1f y=%1.1f z=%1.6f' % (x, y, z)
                    return 'i=%d j=%d z=%.6f' % (col, row, z)
                else:
                    #return 'x=%1.4f, y=%1.4f' % (x, y)
                    return 'outside data area'

            elif bruteforce_flag == '2DXY' and bruteforce_dims[-1] and bruteforce_dims[-2]:
                '''
                our extend in X=[-0.5:numcols-0.5], Y=[-0.5:numrows-0.5], because col,row is cell center!
                '''
                if col >= 0 and col < numcols and row >= 0 and row < numrows:
                    #z = data2d[row, col]
                    # sice we have artificially reversed y-coordinate axes, now reverse data!
                    # numrows-1, because if nrows=10 => row=[0:9]
                    z = data2d[numrows-1-row, col]
                    real_x, real_y = self.get_real_xy(x, y, bruteforce_dims)

                    #return 'x=%1.1f y=%1.1f z=%1.6f' % (x, y, z)
                    #return 'i=%d j=%d z=%.3f x=%.4f y=%.4f' % (col, row, z, real_x, real_y)
                    return 'i=%d j=%d z=%.3f, %s=%.2f %s=%.2f' % (
                        col, row, z, bruteforce_dims[-1], real_x, bruteforce_dims[-2], real_y)
                else:
                    #return 'x=%1.4f, y=%1.4f' % (x, y)
                    return 'outside data area'
            else:
                raise ValueError('bruteforce_flag can be $None$ or $"2DXY"$. Passed %s' % bruteforce_flag)
        ax.format_coord = format_coord


    def allow_menu(self):
        allow = False
        #print "self.mainWidget.get_plotType():", self.mainWidget.get_plotType()
        #print "self.mainWidget.get_plotType_for_timeseries():", self.mainWidget.get_plotType_for_timeseries()
        if self.mainWidget.get_plotType() == "2D" and not self.mainWidget.get_plotType_for_timeseries() == "2DZT":
            allow = True
        return allow
    

    def get_real_xy(self, i, j, dimension_list):
        '''
        functions returns values of x,y based on passed indexes i, j

        '''
        if any(dimension_list[-2:-1]) is None:
            print 'Dimensions X,Y of current variable are not specified (variables that have same name as dimensions not found)'
            raise ValueError('Dimensions X,Y of current variable are not specified (variables that have same name as dimensions not found)')
        nc = self.mainWidget.get_selected_ncfile_instance()
        try:
            x_var = nc.variables[dimension_list[-1]]
            y_var = nc.variables[dimension_list[-2]]
            
        except:
            
            print ('Failed to load variables: {0}, {1}'.format(dimension_list[-1], dimension_list[-2]))
            raise ValueError('Failed to load variables: {0}, {1}'.format(dimension_list[-1], dimension_list[-2]))


        x_ratio = (x_var[-1]-x_var[0])/(len(x_var)-1)
        y_ratio = (y_var[-1]-y_var[0])/(len(y_var)-1)

        #x[i] = x_var[0]+x_ratio*i
        #y[j] = y_var[0]+y_ratio*j
        x = x_var[0] + x_ratio*i
        y = y_var[0] + y_ratio*j
        nc.close()
        return (x, y)

    def get_axes(self):
        return self.fig.get_axes()
    

    def fast_redraw(self, artist):
        background = [self.canvas.copy_from_bbox(self.get_axes()[0].bbox)]
        self.get_axes()[0].draw_artist(artist)
        self.canvas.restore_region(background)
        self.canvas.blit(self.get_axes()[0].bbox)
        self.canvas.update()
        self.canvas.flush_events()



def main():
    app = QApplication(sys.argv)
    form = mpl_widget()
    form.show()
    app.exec_()

if __name__ == "__main__":
    main()
