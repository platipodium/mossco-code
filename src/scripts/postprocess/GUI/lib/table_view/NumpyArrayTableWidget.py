# -----------------------------------------------------------------------------
# > @brief NCINFO - Visualization tool for NOSSCO-output files
#
#   This computer program is part of MOSSCO.
# > @copyright Copyright (C) 2013, 2014, 2015 Bundesanstalt fuer Wasserbau
# > @author
#       Copyright (C) 2004-2009 European Synchrotron Radiation Facility
#       This file is part of the PyMCA X-ray Fluorescence Toolkit developed at
#       the ESRF by the Beamline Instrumentation Software Support (BLISS) group.
# > @modified Nikolai Chernikov, <nikolai.chernikov.ru@gmail.com>
#
#  MOSSCO is free software: you can redistribute it and/or modify it under the
#  terms of the GNU General Public License v3+.  MOSSCO is distributed in the
#  hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
#  LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
# ------------------------------------------------------------------------------


from PyQt4 import QtCore, QtGui
import FrameBrowser
import NumpyArrayTableView

class NumpyArrayTableWidget(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QTableWidget.__init__(self, parent)
        self.mainLayout = QtGui.QVBoxLayout(self)
        self.mainLayout.setMargin(0)
        self.mainLayout.setSpacing(0)
        self.browser = FrameBrowser.FrameBrowser(self)
        self.view = NumpyArrayTableView.NumpyArrayTableView(self)
        self.mainLayout.addWidget(self.browser)
        self.mainLayout.addWidget(self.view)
        self.connect(self.browser,
                     QtCore.SIGNAL("indexChanged"),
                     self.browserSlot)

    def setArrayData(self, data):
        self._array = data
        self.view.setArrayData(self._array)
        if len(self._array.shape) > 2:
            self.browser.setNFrames(self._array.shape[0])
        else:
            self.browser.setNFrames(1)

    def browserSlot(self, ddict):
        if ddict['event'] == "indexChanged":
            if len(self._array.shape) == 3:
                self.view.setCurrentArrayIndex(ddict['new']-1)
                self.view.reset()
"""
if __name__ == "__main__":
    import numpy
    a = QtGui.QApplication([])
    d = numpy.random.normal(0,1, (5, 1000,1000))
    for i in range(5):
        d[i, :, :] += i
    #m = NumpyArrayTableModel(numpy.arange(100.), fmt="%.5f")
    #m = NumpyArrayTableModel(numpy.ones((100,20)), fmt="%.5f")
    w = NumpyArrayTableWidget()
    w.setArrayData(d)
    #m.setCurrentIndex(4)
    #m.setArrayData(numpy.ones((100,100)))
    w.show()
    a.exec_()"""
