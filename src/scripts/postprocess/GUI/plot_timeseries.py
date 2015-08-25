import matplotlib.pyplot as plt
import numpy as np
from netCDF4 import Dataset
from PyQt4.QtGui import QMessageBox
import datetime
import re
from matplotlib.dates import DateFormatter


def plot_StandaloneTimeseriesPlot(mainWidget, points, t1, t2, ax=None, plotType_for_timeseries=None):
    '''
    mainWidget - instance of the mainwidget = in file dataselect_dlg.pyw
    points - list of selected points [p1, p2, p3...] where p1=[i1, j1], p2=[i2, j2], etc.
    t1, t2 - time range for timeseries plots
    ax - matplotlib axes to plot data
    plotType_for_timeseries - argument to detect from which section points were picked
    '''

    ax0 = ax
    nc = Dataset(mainWidget.get_current_nc_fname(), mode='r')
    var = nc.variables[mainWidget.get_current_var_name()]
    

    # use real time dims...
    real_time = False
    try:
        pat_start_time = re.compile('.*?(\d\d\d\d.\d\d.\d\d.\d\d.\d\d.\d\d).*?', re.DOTALL)
        t_start = re.match(pat_start_time, nc.variables['time'].units).group(1)
        t_start = re.sub('[:;/,\.\-]', '/', t_start)
        t_start = datetime.datetime.strptime(t_start, '%Y/%m/%d %H/%M/%S')
        time = [t_start + datetime.timedelta(seconds=i) for i in nc.variables['time']]
        real_time = True
    except Exception as e:
        print e
        QMessageBox.question(mainWidget, 'Warning', "Time units are not recognized. Timeseries is plotted over the values stored in variables 'time'", QMessageBox.Ok)
        time = nc.variables['time'][:]




    _t1, _t2, _z1, _z2, _y1, _y2, _x1, _x2, _ndims, _isSelectedRegion, _isSelectedPoint, _dimensionIsPresent = mainWidget.get_values_to_select_data()

    z1, z2, y1, y2, x1, x2 = _z1, _z2, _y1, _y2, _x1, _x2
    ndims = _ndims
    isSelectedRegion = [True, False, False, False]
    dimensionIsPresent = _dimensionIsPresent
        
    isSelectedPoint = list()
    for region, present in zip(isSelectedRegion, dimensionIsPresent):
        if present:
            isSelectedPoint.append((not region))
        else:
            isSelectedPoint.append(False)



    # now loop over points
    data = []
    for p in points:
        print p
        i, j = p[0], p[1]
        if plotType_for_timeseries == '2DXY':
            # can be: t,z,y,x   t,y,x   z,y,x
            x1, x2 = i, i+1
            y1, y2 = j, j+1

        elif plotType_for_timeseries == '2DZY':
            y1, y2 = i, i+1
            z1, z2 = j, j+1

        elif plotType_for_timeseries == '2DZX':
            x1, x2 = i, i+1
            z1, z2 = j, j+1


        custom_selection = [t1, t2, z1, z2, y1, y2, x1, x2, ndims, isSelectedRegion, isSelectedPoint, dimensionIsPresent]
        try:
            array, dimsSelectedRegions = mainWidget.detectPlotType_andReturnData(var, manually=custom_selection, detect_plot_type=False)
        except:
            QMessageBox.question(mainWidget, 'Warning',
            "The passed time to slice {0}:{1} is invalid.\nCheck you data.\nI will try to plot only first 4 timesteps".format(t1, t2), QMessageBox.Ok)
            custom_selection = [0, 4, z1, z2, y1, y2, x1, x2, ndims, isSelectedRegion, isSelectedPoint, dimensionIsPresent]
            array, dimsSelectedRegions = mainWidget.detectPlotType_andReturnData(var, manually=custom_selection, detect_plot_type=False)
        data.append([array, z1, y1, x1])





    if ax0 is None:
        f = plt.figure()
        ax = f.add_subplot(111)

    try:
        ax.set_ylabel(var.units)
    except:
        ax.set_ylabel('???')



    n_lines = len(data)
    # Have a look at the colormaps here and decide which one you'd like:
    # http://matplotlib.org/1.2.1/examples/pylab_examples/show_colormaps.html
    colormap = plt.cm.gist_ncar
    ax.set_color_cycle([colormap(i) for i in np.linspace(0, 0.9, n_lines)])
    # Plot several different functions...
    labels = []
    for d in data:
        print len(d[0]), len(time)
        ax.plot(time, d[0])
        labels.append('cell: z={0}, y={1}, x={2}'.format(d[1], d[2], d[3]))
    # I'm basically just demonstrating several different legend options here...
    ax.legend(labels, loc='best',
               labelspacing=0.0,
               handletextpad=0.0, handlelength=1.5,
               fancybox=True, shadow=True)

    ax.set_title('Timeseries: '+mainWidget.get_current_var_name())
    if not real_time: ax.set_xlabel(nc.variables['time'].units)

    ax.autoscale_view()
    
    # format the coords message box
    def value(x): return '%1.3f'%x
    ax.fmt_xdata = DateFormatter('%Y/%m/%d %H:%M:%S')
    ax.fmt_ydata = value
    ax.grid(True)

    
    
    if ax0 is None: print 'showing figure' ; f.show()

    nc.close()
        