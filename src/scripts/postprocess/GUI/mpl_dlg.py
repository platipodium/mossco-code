from PyQt4.QtGui import QWidget
import ui_mpl_default_dlg



class Mpl_dlg(QWidget, ui_mpl_default_dlg.Ui_Dialog):
    def __init__(self, parent=None):
        super(Mpl_dlg, self).__init__()
        self.parent = parent
        self.setupUi(self)

    def redraw(self):
        self.mplwidget.canvas.draw()

    def get_axes(self):
        return self.mplwidget.get_axes()
