from PyQt4.QtCore import pyqtSignature, QString
from PyQt4.QtGui import QDialog
import ui_cbar_dialog


class cbarRange_dlg(QDialog, ui_cbar_dialog.Ui_Dialog):
    def __init__(self, parent=None, cbar=None):
        super(cbarRange_dlg, self).__init__(parent)
        self.setupUi(self)
        self.parent = parent
        self.setMin(cbar.cbar.norm.vmin)
        self.setMax(cbar.cbar.norm.vmax)


    @pyqtSignature("")
    def on_pushButton_resetRange_clicked(self):
        vmin, vmax = self.parent.selected_data['vmin'], self.parent.selected_data['vmax']
        if all((vmin, vmax)) is not None:
            self.setMin(vmin)
            self.setMax(vmax)


    def setMin(self, value):
        v = '{0:.5f}'.format(value)
        self.lineEdit_min.setText(QString(''))
        if v not in [None, ' ', '']:
            self.lineEdit_min.setText(QString(v))

    def setMax(self, value):
        v = '{0:.5f}'.format(value)
        self.lineEdit_max.setText(QString(''))
        if v not in [None, ' ', '']:
            self.lineEdit_max.setText(QString(v))

    def getMin(self):
        return float(str(self.lineEdit_min.text()))

    def getMax(self):
        return float(str(self.lineEdit_max.text()))
