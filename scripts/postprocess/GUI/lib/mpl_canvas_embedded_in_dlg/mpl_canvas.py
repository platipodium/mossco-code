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
from matplotlib import gridspec
from PyQt4.QtCore import Qt
from PyQt4.QtGui import QApplication, QWidget, QPushButton, QVBoxLayout, QHBoxLayout
#import mpldatacursor


import inspect, os, sys
relative_paths = [

    '../colorbar_dialog',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)


from colorbar_dlg import cbarRange_dlg







class myNavigationToolbar(NavigationToolbar):
    # only display the buttons we need
    toolitems = [t for t in NavigationToolbar.toolitems if
                 t[0] in ('Home', 'Back', 'Forward', 'Pan', 'Zoom', 'Save')]


class mpl_widget(QWidget):
    def __init__(self, parent=None, grid=[1, 1], cbar=False, enable_data_cursor=False, change_coord_fmt=True):
        QWidget.__init__(self, parent)
        self.parent = parent
        self.create_main_frame(grid=grid, cbar=cbar, enable_data_cursor=enable_data_cursor, change_coord_fmt=change_coord_fmt)
        self.pcm = None


    def create_main_frame(self, grid=[1, 1], cbar=False, enable_data_cursor=False, change_coord_fmt=True):
        print grid
        self.fig = Figure(dpi=100)
        self.canvas = FigureCanvas(self.fig)
        self.canvas.setParent(self)
        self.canvas.setFocusPolicy( Qt.ClickFocus )
        self.canvas.setFocus()
        self.mpl_toolbar = myNavigationToolbar(self.canvas, self)

        # make, add axes subplots
        gs = gridspec.GridSpec(grid[0], grid[1])
        self.axes = list()
        for cell in gs:
            self.axes.append(self.fig.add_subplot(cell))



        if cbar:
            self.cbar_button = QPushButton("Color Range")
            self.cbar_button.setFocusPolicy( Qt.NoFocus )
            self.cbar_button.clicked.connect(self.on_cbar_button_clicked)

        vbox = QVBoxLayout()
        hbox = QHBoxLayout()

        vbox.addWidget(self.canvas)  # the matplotlib canvas
        hbox.addWidget(self.mpl_toolbar)
        if cbar:
            hbox.addWidget(self.cbar_button)
        vbox.addLayout(hbox)
        self.setLayout(vbox)
        
        if change_coord_fmt: self.change_coordinate_formatter(enable_data_cursor=enable_data_cursor)

    def on_cbar_button_clicked(self):
        ''' See interactive
            http://stackoverflow.com/questions/5611805/using-matplotlib-slider-widget-to-change-clim-in-image
        '''
        dialog = cbarRange_dlg(self)
        if dialog.exec_():  # if accepted
            cbar_min = dialog.getMin()
            cbar_max = dialog.getMax()
            self.set_clim([cbar_min, cbar_max])
            self.redraw()
    


    def change_coordinate_formatter(self, enable_data_cursor=False):
        ''' see http://stackoverflow.com/questions/14754931/matplotlib-values-under-cursor
        '''

        def format_coord(x, y):
            return 'x=%.3f y=%.3f' % (x, y)

        for ax in self.get_axes():
            ax.format_coord = format_coord

        if enable_data_cursor:
            pass
            #mpldatacursor.datacursor(hover=True, bbox=dict(alpha=1, fc='w'))

    def redraw(self):
        self.fig.canvas.draw()

            

    def set_clim(self, clim):
        if self.pcm:
            self.pcm.set_clim(vmin=clim[0], vmax=clim[1])
            self.redraw()
            
    def set_plocormesh_object(self, pcm_object):
        self.pcm = pcm_object

    def set_imshow_object(self, imshow_obj):
        self.im = imshow_obj
    
    def set_colorbar(self, cb):
        self.cb = cb

    def set_colorbar_minmax(self, vmin, vmax):
        self.cb.norm.vmin = vmin
        self.cb.norm.vmax = vmax
        self.redraw_cb()

    def redraw_cb(self):
        self.cb.draw_all()
        self.cb.patch.figure.canvas.draw()
        self.redraw()


    def get_colorbar(self):
        return self.cb


    def get_imshow_object(self):
        return self.im

    def get_clim(self):
        return self.pcm.get_clim()

    def get_axes(self):
        return self.axes


def main():
    app = QApplication(sys.argv)
    form = mpl_widget()
    form.show()
    app.exec_()

if __name__ == "__main__":
    main()
