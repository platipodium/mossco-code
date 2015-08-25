from PyQt4 import QtCore, QtGui
import NumpyArrayTableModel
import numpy as np



class HorizontalHeader(QtCore.QAbstractItemModel):
    def __init__(self, parent=None):
        QtGui.QHeaderView.__init__(self, parent)

    def columnCount(self, modelIndex):
        return self.parent().columnCount()

    def headerData(self, section, orientation, role=QtCore.Qt.DisplayRole):
        if role == QtCore.Qt.DisplayRole:
            return QtCore.QVariant("%d" % section)
        return QtCore.QVariant()


class VerticalHeader(QtCore.QAbstractItemModel):
    def __init__(self, parent=None):
        QtGui.QHeaderView.__init__(self, parent)

    def rowCount(self, modelIndex):
        return self.parent().rowCount()

    def headerData(self, section, orientation, role=QtCore.Qt.DisplayRole):
        
        # --------------------------------------------
        # swap vertical indexing (from 0--N to N--0)
        nRows = self.parent().rowCount()
        headerLabels = np.arange(nRows-1, -1, -1)
        if role == QtCore.Qt.DisplayRole:
            return unicode(headerLabels[section])
        # --------------------------------------------
        
        #if role == QtCore.Qt.DisplayRole:
        #    return QtCore.QVariant("%d" % section)
        #return QtCore.QVariant()


class NumpyArrayTableView(QtGui.QTableView):
    def __init__(self, parent=None):
        QtGui.QTableView.__init__(self, parent)
        self._model = NumpyArrayTableModel.NumpyArrayTableModel(self)
        self.setModel(self._model)
        horizontalHeaderModel = HorizontalHeader(self._model)
        verticalHeaderModel = VerticalHeader(self._model)
        self.horizontalHeader().setModel(horizontalHeaderModel)
        self.verticalHeader().setModel(verticalHeaderModel)

    def setArrayData(self, data):
        self._model.setArrayData(data)
        #some linux distributions need this call
        self.setModel(self._model)
        horizontalHeaderModel = HorizontalHeader(self._model)
        verticalHeaderModel = VerticalHeader(self._model)
        self.horizontalHeader().setModel(horizontalHeaderModel)
        self.verticalHeader().setModel(verticalHeaderModel)

        
    def setCurrentArrayIndex(self, index):
        return self._model.setCurrentArrayIndex(index)

    def get_currentItemModel(self):
        return self._model

"""
if __name__ == "__main__":
    import numpy
    a = QtGui.QApplication([])
    d = numpy.arange(12).reshape(3,4)

    w = NumpyArrayTableView()
    w.setArrayData(d)

    w.show()
    a.exec_()
"""
