from PyQt4.QtCore import QString, pyqtSignature
from PyQt4.QtGui import QDialog
import ui_flux_tab_widget

import matplotlib.pyplot as plt
import numpy as np

# use this if you want to include modules from a subfolder
import sys, os, inspect
relative_paths = [
    '../plot',
    '../main_dialog',
]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

import plot_2D_fluxes


def checkLineEditText(lineEdit, maxValue, update_lineedit=True):
        text = unicode(lineEdit.text())
        if text:
            try:  # if we have only one timestep
                selected_t = int(text)
                if selected_t < 0:
                    selected_t = 0
                if selected_t > maxValue:
                    selected_t = maxValue
                if update_lineedit: lineEdit.clear()
                if update_lineedit: lineEdit.setText(QString("{0}".format(selected_t)))
                return (selected_t,)

            except ValueError:  # if we cannot convert to integer
                try:  # if we have a region ( int:int )
                    first_t, last_t = text.split(":")[0:2]
                    f_t = int(first_t)
                    l_t = int(last_t)
                    if f_t < 0: f_t = 0
                    if l_t > maxValue: l_t = maxValue
                    if f_t == l_t:  # we dont want to have regions like (3:3)
                        if update_lineedit: lineEdit.clear()
                        if update_lineedit: lineEdit.setText(QString("{0}".format(f_t)))
                        return (f_t,)
                except ValueError:  # wrong user input!
                    f_t = 0
                    l_t = maxValue

                if update_lineedit: lineEdit.clear()
                if update_lineedit: lineEdit.setText(QString("{0}:{1}".format(f_t, l_t)))
                return (f_t, l_t)



class plot_flux_widget_dlg(QDialog, ui_flux_tab_widget.Ui_Dialog):
    def __init__(self, parent=None, data=None, plot=False):
        '''data = [t, p1, p2, lats, lons, layerh4D, vel4D, spm4D]
        '''
        


        #'''
        #data is a dictionary, with keys ['vel2d', 'spm2d', 'thickness2d', 'area2d', 'flux2d' ....]
        #and values as 2D arrays
        #'''


        super(plot_flux_widget_dlg, self).__init__(parent)
        self.parent = parent
        self.setupUi(self)
        self.init_tooltips()
        self._dataStart = data
        self.label_point1.setText('({p[0]}, {p[1]})'.format(p=self._dataStart['p1']))
        self.label_point2.setText('({p[0]}, {p[1]})'.format(p=self._dataStart['p2']))
        
        self.calculate_1st_time()
        self.make_plot()
        

        self.lineEdit.setText('0:{0}'.format(self.parent.parent.get_nTimesteps()))
        self.run_all_calculations()
       
        self.show()



    def init_tooltips(self):
        self.comboBox.setToolTip('Select a timestep to investigate it further. For the selected timestep all other Tabs will be updated')
        self.pushButton.setToolTip('Select timestep range in lineEdit and click this button')
        self.lineEdit.setToolTip('Select timestep range as <i>t1:t2</i> or single timestep as <i>t1</i>')




    def get_t_flux_dt_mass(self):
        t  = self._data['t']  # timestep index, integer [-]
        f  = plot_2D_fluxes.get_sum_flux(self._data['flux2d'])   # mass flux [g/s], integrated
        dt = plot_2D_fluxes.get_timestep_duration(self._data['t'], self._dataStart['time'])  # timestep duration [s]
        m  = plot_2D_fluxes.get_mass(f, dt)  # flux-mass, float, [g]
        return (t, f, dt, m)
    

        
    def populate_comboBox(self, data):
        ''' data = (t, f, dt, m)
        see    >>> get_t_flux_dt_mass()
        '''
        name = 't={0}, dt={1:.0f} sec, flux={2:.3f} g/s, m={3:.3} g'.format(data[0], data[2], data[1], data[3])
        self.comboBox.addItem(unicode(name))


    def populate_textBrowser(self, data):
        ''' data = (t, f, dt, m)
        see    >>> get_t_flux_dt_mass()
        '''
        line = '| {0:>5} | {1:>8} | {2:>20} | {3:>20} |'.format(data[0], '{:.0f}'.format(data[2]), '{:.5f}'.format(data[1]), '{:.5f}'.format(data[3]))
        self.textBrowser.append(QString(line))

    

    def print_header_in_textBrowser(self):
        self.textBrowser.clear()
        line1 = '-'*(8+11+18+18+11)
        line3 = '| {0:^5} | {1:^8} | {2:^20} | {3:^20} |'.format('t', 'dt [sec]', 'flux at t [g/s]', 'mass at t [g]')
        line5 = '-'*(8+11+18+18+11)
        
        for line in [line1, line3, line5]:
            self.textBrowser.append(QString(line))
    


    def print_tail_in_textBrowser(self, m):
        line1 = '-'*(8+11+18+18+11)
        line3 = '| {0} {1} {2} mass sum = {3:.5f} [g]|'.format(' '*5, ' '*8, ' '*15, m)
       
        for line in [line1, line3]:
            self.textBrowser.append(QString(line))



    def calculate_1st_time(self):
        self.comboBox.clear()
        
        self._data = plot_2D_fluxes.get_data_to_plot(self._dataStart['t'], self._dataStart['p1'], self._dataStart['p2'], self._dataStart['lats'],
                            self._dataStart['lons'], self._dataStart['layerh4D'], self._dataStart['vel4D'], self._dataStart['spm4D'])
        del self._dataStart['layerh4D']



    
    def calculate_not1st_time(self, t):
        _data = plot_2D_fluxes.get_data_to_plot2(t, self._dataStart['p1'], self._dataStart['p2'], self._data['thickness2d'],
                            self._data['length_2D'], self._dataStart['vel4D'], self._dataStart['spm4D'])
        self._data['vel2d']  = _data['vel2d']
        self._data['spm2d']  = _data['spm2d']
        self._data['flux2d'] = _data['flux2d']
        self._data['title']  = _data['title']
        self._data['clim']   = _data['clim']
        
    



    def update_tables(self):
        self.set_table_velocity()
        self.set_table_spm()
        self.set_table_cellthickness()
        self.set_table_area()
        self.set_table_massflux()


    def flipud_data2d(self):
        for key, d in self._data.iteritems():
            if isinstance(d, (np.ndarray, np.generic) ):
                if len(d.shape) == 2:
                    #print 'flipping data:', key
                    self._data[key] = np.flipud(d)


    def set_table_velocity(self):
        data2D = self.get_data()['vel2d']
        self.tableView_1.setArrayData(data2D)

    def set_table_spm(self):
        data2D = self.get_data()['spm2d']
        self.tableView_2.setArrayData(data2D)

    def set_table_cellthickness(self):
        data2D = self.get_data()['thickness2d']
        self.tableView_3.setArrayData(data2D)

    def set_table_area(self):
        data2D = self.get_data()['area2d']
        self.tableView_4.setArrayData(data2D)

    def set_table_massflux(self):
        data2D = self.get_data()['flux2d']
        self.tableView_5.setArrayData(data2D)

    def get_data(self):
        return self._data

    def get_ax(self):
        return self.mplwidget.get_axes()[0]






    def make_plot(self, units='g/s', only_update=False):
        self.flipud_data2d()
        self.update_tables()


        ax = self.get_ax()
        #xx = self.get_data()['coord2d']
        #yy = self.get_data()['elev_2d']
        data = np.flipud(self.get_data()['flux2d'])
        clim = self.get_data()['clim']
        title = self.get_data()['title']

        ax.set_title(title)

        if not only_update:
            im = ax.imshow(data, vmin=clim[0], vmax=clim[1], aspect='auto')
            self.mplwidget.set_imshow_object(im)
            cb = plt.colorbar(im, ax=ax, label=units)
            self.mplwidget.set_colorbar(cb)

            ax.invert_yaxis()
            #ax.set_ylabel("elevation [m BMSL]")
            ax.set_ylabel("Grid Z")

            #scale = 0.001  #converting from [m] into [km]
            #ticks = ticker.FuncFormatter(lambda x, pos: "{0:g}".format(x*scale))
            #ax.xaxis.set_major_formatter(ticks)
            #ax.set_xlabel("distance [km]")
            if self._dataStart['p1'][1] == self._dataStart['p2'][1]:
                ax.set_xlabel("Grid X")
            elif self._dataStart['p1'][0] == self._dataStart['p2'][0]:
                ax.set_xlabel("Grid Y")
        else:
            im = self.mplwidget.get_imshow_object()
            im.set_data(data)
            self.mplwidget.set_colorbar_minmax(data.min(), data.max())
        self.mplwidget.redraw()

    def run_all_calculations(self):
        t = checkLineEditText(self.lineEdit, self.parent.parent.get_nTimesteps(), update_lineedit=True)
        
        if len(t) == 1:
            timesteps = t[0]
        else:
            timesteps = np.arange(t[0], t[1], 1)


        self.comboBox.clear()
        self.print_header_in_textBrowser()
        M = 0
        for t in timesteps:
            self._data['t'] = t
            self.calculate_not1st_time(t)
            values = self.get_t_flux_dt_mass()
            M += values[3]
            self.populate_comboBox(values)
            self.populate_textBrowser(values)
        self.print_tail_in_textBrowser(M)



    @pyqtSignature("")
    def on_pushButton_clicked(self):
        self.run_all_calculations()


    @pyqtSignature("int")
    def on_comboBox_activated(self, index):
        self.calculate_not1st_time(index)
        self.make_plot(only_update=True)
