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

import netCDF4
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import ticker
import inspect, os, sys
relative_paths = [

    '..',

]
for p in relative_paths:
    cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(
                        inspect.getfile( inspect.currentframe() ))[0], p)))
    if cmd_subfolder not in sys.path:
        sys.path.insert(0, cmd_subfolder)

import FUNCTIONS


def get_data_to_plot2(t, p1, p2, layer_thickness_2D, length_2D, vel4D, spm4D):
    title = 'Mass flux. Section point: p1=({0}), p2=({1}). Timestep={2}'.format(p1, p2, t)
    ii, ij = get_cells_indexes(p1, p2)
    
    vel2D = create_values_2D(vel4D, ii, ij, t)
    spm2D = create_values_2D(spm4D, ii, ij, t)

    
    # du multiplication
    area, massflux = multiply_arrays(vel2D, spm2D, length_2D, layer_thickness_2D, log=False)


    DATA = dict()
    DATA['vel2d'] = vel2D
    DATA['spm2d'] = spm2D
    DATA['flux2d'] = massflux
    DATA['title'] = title
    DATA['clim'] = [massflux.min(), massflux.max()]
    
    return DATA


def get_data_to_plot(t, p1, p2, lats, lons, layerh4D, vel4D, spm4D, clim=[0, 100]):

    '''
    COPY STUFF FROM 3d plotting.....
    '''
    print 'starting main plot_flux()'
    title = 'Mass flux. Section point: p1=({0}), p2=({1}). Timestep={2}'.format(p1, p2, t)
    n_layers = spm4D.shape[-3]
    
    ii, ij = get_cells_indexes(p1, p2)
    length_1D = get_cells_length(ii, ij, lats, lons)
    length_2D = create_length_2D(n_layers, length_1D)
    coords_2D = create_cellCenterCoord_2D(length_2D)
    
    vel2D = create_values_2D(vel4D, ii, ij, t)
    spm2D = create_values_2D(spm4D, ii, ij, t)

    layer_thickness_2D = create_values_2D(layerh4D, ii, ij, t)
    #lh_2D = create_values_2D(layerh4D, ii, ij, t)
    #layer_cell_elev_2D, layer_thickness_2D = get_layer_info_2D(lh_2D, log=True)

    # du multiplication
    area, massflux = multiply_arrays(vel2D, spm2D, length_2D, layer_thickness_2D, log=True)


    DATA = dict()
    DATA['vel2d'] = vel2D
    DATA['spm2d'] = spm2D
    DATA['thickness2d'] = layer_thickness_2D
    DATA['length_2D'] = length_2D
    DATA['area2d'] = area
    DATA['flux2d'] = massflux
    DATA['coord2d'] = coords_2D
    #DATA['elev_2d'] = layer_cell_elev_2D
    DATA['title'] = title
    DATA['clim'] = [massflux.min(), massflux.max()]
    
    return DATA



def get_sum_flux(massflux2d):
    return massflux2d.sum()


def get_timestep_duration(t, time_var):
    ''' timestep duration between this timestep and previous
        
        time_var -array with "seconds since some_date"
        t - integer, pointer to current timestep
    '''
    if t == 0:
        return 0
    else:
        return time_var[t] - time_var[t-1]


def get_mass(flux, dt):
    ''' flux - float in [g/s]
        dt - float in [s]
    '''
    return flux*dt


def main(t, p1, p2, lats, lons, layerh4D, vel4D, spm4D, clim=[0, 100]):
    DATA = get_data_to_plot(t, p1, p2, lats, lons, layerh4D, vel4D, spm4D, clim=clim)
    makeplots(DATA['coord2d'], DATA['elev_2d'], DATA['flux2d'], DATA['title'], clim=DATA['clim'])



def makeplots(xx, yy, data, title, clim, units='g/s', ax=None):

    if not ax:
        print 'creating new figure'
        f = plt.figure(figsize=(8, 4), dpi=100)
        ax1 = f.add_subplot(111)
    else:
        ax1 = ax

    ax1.text(0.5, 0.96, title, horizontalalignment='center')

    pcm = ax1.pcolormesh(xx, yy, data, vmin=clim[0], vmax=clim[1])
    

    plt.colorbar(pcm, ax=ax, label=units)
    ax1.invert_yaxis()
    ax1.set_ylabel("elevation [m BMSL]")

    scale = 0.001  #converting from [m] into [km]
    ticks = ticker.FuncFormatter(lambda x, pos: "{0:g}".format(x*scale))
    ax1.xaxis.set_major_formatter(ticks)
    ax1.set_xlabel("distance [km]")
    
    if not ax:
        f.show()
    return pcm




def multiply_arrays(vel, spm, l, h, log=False):
    if log:
        print 'velocity:\n'+'-'*50
        print vel
        print 'spm:\n'+'-'*50
        print spm
        print 'cell length:\n'+'-'*50
        print l
        print 'cell thickness:\n'+'-'*50
        print h
    a1 = np.multiply(l, h)
    a2 = np.multiply(vel, spm)
    a3 = np.multiply(a1, a2)
    if log:
        print 'area:\n'+'-'*50
        print a1
        print 'mass flux per sq.m:\n'+'-'*50
        print a2
        print 'mass flux per cell:\n'+'-'*50
        print a3
    return a1, a3


def get_layer_height_2D(filename, varname, ii, ij, t):
    nc = netCDF4.Dataset(filename)
    var = nc.variables[varname]
    if len(varname.shape) != 4:
        raise ValueError('height variable {0} has invalid shape {1}. Should be a 4D var'.format(varname, var.shape))
    
    for i, j, index in zip(ii, ij, np.arange(len(ii))):
        column = data3D[t, :, j, i]
        if index == 0:
            data_2D = column
        else:
            data_2D = np.vstack((data_2D, column))
    nc.close()
    return data_2D.T


def get_layer_info_2D(layer_height_2D, log=False):
    '''
    layer_height_2D - a 2D array, in x - vector length, in y - number of layers+1
    values represent elevation of the bottom of the layer. The last value in a column represents elevation
    of the top of last cell
    '''

    layer_thickness_2D = np.empty((layer_height_2D.shape[0]-1, layer_height_2D.shape[1]))
    layer_cell_elev_2D = np.empty((layer_height_2D.shape[0]-1, layer_height_2D.shape[1]))

    for x in xrange(layer_height_2D.shape[1]):
        for z in xrange(layer_height_2D.shape[0]-1):
            layer_cell_elev_2D[z, x] = (layer_height_2D[z, x]+layer_height_2D[z+1, x])/2.
            layer_thickness_2D[z, x] = abs(layer_height_2D[z, x]-layer_height_2D[z+1, x])
    if log:
        print 'layer_cell_ center elevation ...\n'+'-'*50
        print layer_cell_elev_2D
        print 'layer_cell thickness ...\n'+'-'*50
        print layer_thickness_2D
    return layer_cell_elev_2D, layer_thickness_2D


def create_length_2D(n_layers, length_1D):
    length_2D = np.tile(length_1D, (n_layers, 1))
    return length_2D


def create_cellCenterCoord_2D(length2D):
    cellCoord_2D = np.empty(length2D.shape)
    c1 = 0.
    for i in xrange(length2D.shape[1]):
        if i == 0:  # skipping first point... cause c1=0.
            cellCoord_2D[:, i] = 0
        else:
            dx = length2D[0, i-1]/2. + length2D[0, i]/2.
            c_i = cellCoord_2D[0, i-1] + dx
            cellCoord_2D [:, i] = c_i
    return cellCoord_2D


def create_values_2D(data, ii, ij, t=None):
    '''
    data can be 3D or 4D [t, z, y, x] or [z, y, x]
    '''

    if t is not None and len(data.shape) == 4:  #
        for i, j, index in zip(ii, ij, np.arange(len(ii))):
            column = data[t, :, j, i]
            if index == 0:
                data_2D = column
            else:
                data_2D = np.vstack((data_2D, column))
        return data_2D.T

    elif t is None and len(data.shape) == 3:
        for i, j, index in zip(ii, ij, np.arange(len(ii))):
            column = data[:, j, i]
            if index == 0:
                data_2D = column
            else:
                data_2D = np.vstack((data_2D, column))
        return data_2D.T
    else:
        msg = 'Data has shape {0}. Not supported. Should be 3D or 4D'.format(data.shape)
        raise ValueError(msg)




def get_cells_indexes(crd1, crd2):
    '''
    input: two cell coordinates in i, j, where i,j ranges from [0,...N-1], where N is
    the number of cells in current dimension
    assuming that the two point are located on horizontal or vertical line
    returns:
        ii, ij - two 1D arrays. Length = cells selected
        ii - indexes of i
        ij - indexes of j
    '''
    if crd1[1] == crd2[1]:
        #print 'same lon'
        add = (crd2[0]-crd1[0])/abs(crd2[0]-crd1[0])  # this huge bracket yeilds +/-1
        # loop over cells in line
        ii = np.arange(crd1[0], crd2[0]+add, add)
        ij = np.array([crd1[1]+0/i for i in ii])
        return ii, ij

    elif crd1[0] == crd2[0]:
        add = (crd2[1]-crd1[1])/abs(crd2[1]-crd1[1])  # this huge bracket yeilds +/-1
        # loop over cells in line
        ij = np.arange(crd1[1], crd2[1]+add, add)
        ii = np.array([crd1[0]+0/j for j in ij])
        return ii, ij
    else:
        raise ValueError('Two points are not on horizontal or verticall line')



def get_cells_length(ii, ij, lats, lons, log=False):
    '''
    input:
    ii , ij - produced by get_cells_indexes() : arrays of cell indexes in i- and j- direction
    return 1D numpy array of vector length, values represent arc length of cells
    '''
    if log:
        print lats
        print lons
    dlat = lats[1] - lats[0]  # assuming uniform spacing between cells
    dlon = lons[1] - lons[0]  # assuming uniform spacing between cells

    array_length = len(ii)
    length_array = np.empty(array_length)  # ? maybe np.empty([array_length])
    
    if ii[0] == ii[1]:
        print 'same lon'
        lon = lons[ii[1]]
        for n, j in enumerate(ij):
            j_lat = lats[j]  # cell centers
            lat_t = j_lat+dlat/2.  # latitude cell top
            lat_b = j_lat-dlat/2.  # latitude cell bottom
            arc_l = FUNCTIONS.calcVincentyInverse(lat_t, lon, lat_b, lon)[0]
            length_array[n] = arc_l
            if log:
                print 'cell No {0}, index {1}'.format( n, j)
                print 'cell lat bottom ={0}, lat center={1}, lat top={2}'.format(lat_b, j_lat, lat_t)
                print 'arc_length={0}'.format(arc_l)
                print '*'*10
        
        return length_array

    elif ij[0] == ij[1]:
        print 'same lat'
        lat = lats[ij[1]]
        for n, i in enumerate(ii):
            i_lon = lons[i]  # cell centers
            lon_l = i_lon+dlon/2.  # longitude cell left
            lon_r = i_lon-dlon/2.  # longitude cell right
            arc_l = FUNCTIONS.calcVincentyInverse(lat, lon_l, lat, lon_r)[0]
            length_array[n] = arc_l

        return length_array
    else:
        raise ValueError('Two points are not on horizontal or verticall line. Somethings wrong...')


def get_velocity_4D(fname,  ii , ij, vel_x='x_velocity_in_water', vel_y='y_velocity_in_water'):
    nc = netCDF4.Dataset(fname)

    if ii[0] == ii[1]:
        vel = nc.variables[vel_y]
        vel = np.squeeze(vel[:, :, :, :])
    elif ij[0] == ij[1]:
        vel = nc.variables[vel_x]
        vel = np.squeeze(vel[:, :, :, :])
    else:
        raise ValueError('velocity not recognized')

    nc.close()
    return vel


def get_spm_4D(fname, spm_vars=['concentration_of_SPM_in_water_001', 'concentration_of_SPM_in_water_002']):
    nc = netCDF4.Dataset(fname)

    SPM = list()
    for spm_var in spm_vars:
        SPM.append( np.squeeze(nc.variables[spm_var][:, :, :, :]))
    nc.close()
    
    for s in SPM:
        spm_finall += s

    return spm_finall

if __name__=="__main__":


    def getvar_ffile_old(f, varname):
        nc=netCDF4.Dataset(f)
        var=nc.variables[varname]
        val_org = np.squeeze(var[:, :])
        nc.close()
        return val_org


    def get_lan_lon(f):
        nc=netCDF4.Dataset(f)
        latNC=nc.variables['lat']
        lonNC=nc.variables['lon']
        lats = np.squeeze(latNC[:])
        lons = np.squeeze(lonNC[:])
        nc.close()
        return lats, lons

    p1 = [2, 10]
    p2 = [2, 12]
    nt = 2
    t = 0
    n_layers = 5
    nz = n_layers
    fname      = '//net/widar/data/nick/to_do/014_arcMap_map_TF_into_MOSSCO_grid/topo.nc'
    _2D_bathymetry = getvar_ffile_old(fname, 'bathymetry')
    ny, nx = _2D_bathymetry.shape[0], _2D_bathymetry.shape[1]
    lats, lons = get_lan_lon(fname)
    #data1 = np.random.uniform(low=0.0, high=100., size=(n_layers, _2D_bathymetry.shape[0], _2D_bathymetry.shape[1]))
    #for i in range(n_layers):
    #    data1[i, ...] = np.random.uniform(low=20*i, high=20*(i+1), size=(_2D_bathymetry.shape[0], _2D_bathymetry.shape[1]))

    spm = np.empty((nt, n_layers, ny, nx))
    velx = np.empty((nt, n_layers, ny, nx))
    vely = np.empty((nt, n_layers, ny, nx))
    lh = np.empty((nt, n_layers+1, ny, nx))

    for z in range(n_layers):
        spm[:, z, :, :] = 10.*(z-nz)
        velx[:, z, :, :] = 0.2+(0.1*z)
        vely[:, z, :, :] = -0.2-(0.1*z)
        lh[:, z, :, :] = -nz+z
    lh[:, -1, :, :] = 0




    vel4D = vely
    spm4D = spm
    layerh4D = lh



    ii, ij = get_cells_indexes(p1, p2)
       

    #vel4D = get_velocity_4D(fname, ii , ij)
    vel_2D = create_values_2D(vel4D, ii, ij, t)

    #spm4D = get_spm_4D(fname, spm_vars=['concentration_of_SPM_in_water_001', 'concentration_of_SPM_in_water_002'])
    spm_2D = create_values_2D(spm4D, ii, ij, t)

    #layer_height_2D = get_layer_height_2D(filename, 'SPECIALVARNAME', ii, ij, t)
    #layer_cell_elev_2D, layer_thickness_2D = get_layer_info_2D(layer_height_2D)


    from PyQt4.QtCore import *
    from PyQt4.QtGui import *
    app = QApplication(sys.argv)

    main(t, p1, p2, lats, lons, layerh4D, vel4D, spm4D, clim=[0, 100])
    


