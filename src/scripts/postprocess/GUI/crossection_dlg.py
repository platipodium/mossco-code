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

