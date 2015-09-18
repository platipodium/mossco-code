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


from PyQt4.QtGui import QWidget
import ui_crosssection_plot_dlg


class Mpl_dlg(QWidget, ui_crosssection_plot_dlg.Ui_Dialog):
    def __init__(self, parent=None):
        super(Mpl_dlg, self).__init__()
        self.parent = parent
        self.setupUi(self)

    def redraw(self):
        self.mplwidget.canvas.draw()

    def get_axes(self):
        return self.mplwidget.get_axes()

