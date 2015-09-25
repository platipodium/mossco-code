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

from matplotlib.figure import Figure
from matplotlib.backends.backend_qt4agg import (
    FigureCanvasQTAgg as FigureCanvas,
    NavigationToolbar2QT as NavigationToolbar)

from PyQt4.QtCore import Qt
from PyQt4.QtGui import QWidget, QApplication, QPushButton, QVBoxLayout, QHBoxLayout 

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
#from matplotlib.backends import qt_compat



class myNavigationToolbar(NavigationToolbar):
    # only display the buttons we need
    toolitems = [t for t in NavigationToolbar.toolitems if
                 t[0] in ('Home', 'Back', 'Forward', 'Pan', 'Zoom', 'Save')]


class mpl_widget(QWidget):
    def __init__(self, parent=None, mainWidget=None):
        QWidget.__init__(self, parent)
        self.parent = parent
        self.mainWidget = mainWidget
        self.create_main_frame()
        self.pcm = None



    def create_main_frame(self):

        self.fig = Figure(dpi=100)
        self.canvas = FigureCanvas(self.fig)
        self.canvas.setParent(self)
        self.canvas.setFocusPolicy( Qt.ClickFocus )
        self.canvas.setFocus()
        self.ax = self.fig.add_subplot(111)

        self.mpl_toolbar = myNavigationToolbar(self.canvas, self)

        self.cbar_button = QPushButton("Color Range")
        self.cbar_button.setFocusPolicy( Qt.NoFocus )
        self.cbar_button.clicked.connect(self.on_cbar_button_clicked)

        vbox = QVBoxLayout()
        hbox = QHBoxLayout()

        vbox.addWidget(self.canvas)  # the matplotlib canvas
        hbox.addWidget(self.mpl_toolbar)
        hbox.addWidget(self.cbar_button)
        vbox.addLayout(hbox)
        self.setLayout(vbox)

    def set_clim(self, clim):
        if self.pcm:
            self.pcm.set_clim(vmin=clim[0], vmax=clim[1])
            self.redraw()


    def redraw(self):
        self.fig.canvas.draw()


    def on_cbar_button_clicked(self):
        ''' See interactive
            http://stackoverflow.com/questions/5611805/using-matplotlib-slider-widget-to-change-clim-in-image
        '''
        dialog = cbarRange_dlg(self, cbar_min=self.get_clim()[0], cbar_max=self.get_clim()[1])
        if dialog.exec_():  # if accepted
            cbar_min = dialog.getMin()
            cbar_max = dialog.getMax()
            self.set_clim([cbar_min, cbar_max])
            self.redraw()
    
    def set_plocormesh_object(self, pcm_object):
        self.pcm = pcm_object
    
    def get_clim(self):
        return self.pcm.get_clim()

    def get_ax(self):
        return self.ax


def main():
    app = QApplication(sys.argv)
    form = mpl_widget()
    form.show()
    app.exec_()

if __name__ == "__main__":
    main()
