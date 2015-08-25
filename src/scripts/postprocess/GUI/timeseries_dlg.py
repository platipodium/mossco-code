from PyQt4.QtGui import QWidget
import ui_timeseries_plot_dlg


class Mpl_dlg(QWidget, ui_timeseries_plot_dlg.Ui_Dialog):
    def __init__(self, parent=None):
        super(Mpl_dlg, self).__init__()
        self.parent = parent
        self.setupUi(self)


    def toggle_x_axis_datetime(self):
        self.mplwidget.fig.autofmt_xdate()

    def redraw(self):
        self.mplwidget.canvas.draw()

    def get_ax(self):
        return self.mplwidget.get_axes()[0]
