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


from PyQt4 import QtGui, QtCore
import sched, time

import inspect, os, sys
relative_paths = [

    '..',
    '../plot',
    '../timeseries_plot_dialog',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

import FUNCTIONS as f
from plot_timeseries import plot_StandaloneTimeseriesPlot
from timeseries_dlg import Mpl_dlg


class mpl_menu(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.parent = parent
        self.fig = parent.fig
        self.canvas = parent.canvas
        self.canvas.mpl_connect('button_release_event', self.toggle_mpl_menu)

        self.create_menu_actions()
        self.mpl_menu_update()
        self.init_tooltips()
        self.show()


    def init_tooltips(self):
        self.mpl_menu_action_timeseries.setToolTip('Generate timeseries plot for <b>ALL</b> selected cells in seperate window')
        self.mpl_menu_action_picknext.setToolTip('Click here and select next cell with RMB. <br> Selected cells are highlighted with black box')
        self.mpl_menu_action_cancel.setToolTip('Clear cell selection')



    def toggle_mpl_menu(self, event):
        if event.inaxes != self.parent.get_axes()[0]: return
        if self.parent.allow_menu():
            if event.button == 1:  # if lmb is clicked
                pass

            elif event.button == 3 :  #if mmb is clicked
                if self.parent.allow_popup_menu:
                    self.mpl_menu_update()
                    canvasSize = event.canvas.geometry()
                    Qpoint_click = event.canvas.mapToGlobal(QtCore.QPoint(event.x, canvasSize.height()-event.y))
                    self.point = [event.xdata, event.ydata]
                    self.canvasMenu.move(Qpoint_click)
                    self.canvasMenu.show()

        #self.parent.shift_is_held = False  # RELEASE SHIFT button

    def mpl_menu_picknext(self):
        self._pick_next_rmb_cell = True
        self._cancel_selection = False
        self._generate_crosssection = False

        # if "cancel" do not exist yet ...
        #if self.mpl_menu_action_cancel not in self.canvasMenu.actions():



    def mpl_menu_timeseries(self):
        #self.parent.mpl_toolbar.setEnabled(True)  # reactivate toolbar if click on the menu
        print 'creating timeseries plot...'
        mainWidget = self.parent.mainWidget
        points = self.parent.get_selected_cells_ij()
        t1, t2 = 0, mainWidget.get_nTimesteps()

        dlg = Mpl_dlg()
        ax = dlg.get_ax()
        plot_StandaloneTimeseriesPlot(mainWidget, points, t1, t2, plotType_for_timeseries=mainWidget.plotType_for_timeseries, ax=ax)
        dlg.toggle_x_axis_datetime()
        dlg.redraw()
        dlg.show()

    def mpl_menu_cancelpick(self):
        self._cancel_selection = True
        self._pick_next_rmb_cell = False
        self.parent.clear_selection()
    





    def show_tooltip(self):
        print 'showing tooltip'
        action = self.mouse_hover_action
        QtGui.QToolTip.showText(
            QtGui.QCursor.pos(), action.toolTip(),
            self.canvasMenu, self.canvasMenu.actionGeometry(action))
   
    def handleMenuHovered(self, action):
        self.mouse_hover_action = action
        #print action
        timeoutTimer = QtCore.QTimer()
        timeoutTimer.setInterval(500)  # wait for 0.5 secs
        timeoutTimer.setSingleShot(True)
        #print 'connecting'
        timeoutTimer.timeout.connect(self.show_tooltip)
        #print 'starting'
        timeoutTimer.start()
        #print 'finished'  # NOT WORKING WHY



    def create_menu_actions(self):
        self.canvasMenu = QtGui.QMenu()
        self.canvasMenu.hovered.connect(self.handleMenuHovered)

    
        ca = QtGui.QAction('Generate Timeseries plot', self)
        ca.triggered.connect(self.mpl_menu_timeseries)
        self.canvasMenu.addAction(ca)
        self.mpl_menu_action_timeseries = ca

        ca = QtGui.QAction('Pick next cell...', self)
        ca.triggered.connect(self.mpl_menu_picknext)
        self.canvasMenu.addAction(ca)
        self.mpl_menu_action_picknext = ca

        ca = QtGui.QAction('Clear Selection', self)
        ca.triggered.connect(self.mpl_menu_cancelpick)
        self.canvasMenu.addAction(ca)
        self.mpl_menu_action_cancel = ca

        self._pick_next_rmb_cell = False
        self._cancel_selection = False
        self._generate_crosssection = False

    def mpl_menu_update(self):
        self.mpl_menu_action_timeseries.setEnabled(False)
        self.mpl_menu_action_picknext.setEnabled(True)
        self.mpl_menu_action_cancel.setEnabled(True)

        if self.parent.mainWidget.get_tIsPresent():
            self.mpl_menu_action_timeseries.setEnabled(True)

        self._cancel_selection = False
        self._pick_next_rmb_cell = False

    def allow_rmb_select_cells(self):
        return self._pick_next_rmb_cell

    def allow_rmb_clear_cells(self):
        return self._cancel_selection
