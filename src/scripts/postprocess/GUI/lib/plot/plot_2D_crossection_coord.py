# -----------------------------------------------------------------------------
# > @brief from a 2-D geo-referenced field, extracts a transect across 2 specified coordinate points
#
#   This computer program is part of MOSSCO.
# > @copyright Copyright (C) 2013, 2014, 2015 Bundesanstalt fuer Wasserbau
# > @author onur.kerimoglu@hzg.de
# > @modified Nikolai Chernikov, <nikolai.chernikov.ru@gmail.com>
#
#  MOSSCO is free software: you can redistribute it and/or modify it under the
#  terms of the GNU General Public License v3+.  MOSSCO is distributed in the
#  hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
#  LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
# ------------------------------------------------------------------------------

import os
import pickle
import netCDF4
import numpy as np
from scipy import interpolate
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib import ticker
import time



def main(crd1, crd2, fname, fname_topo, varname, data3D, layerHeight3D, arlen=100, PRJ_DATA=None, clim=[0, 100], ax1=None, ax2=None):
    """
    data3D  = 3D numpy array [z,y,x] with data information
    depth3D = 3D numpy array [z,y,x] with elevation of cells
    !!! Note: data3D.shape = depth3D.shape !!!
    """


    fpath = os.path.dirname(fname)
    print "Running main()"
    titlestr = getvar_ffile(fname, varname)


    #get a projection
    print 'get a projection'
    start = time.time()
    proj = get_proj(fpath, PRJ_DATA=PRJ_DATA, bathymetry_fname=fname_topo)
    end = time.time()
    print 'time_elapsed:', end - start



    #original lat&lon (xc&yc) of the grid (centers)
    lon_org, lat_org = get_lonlat_org(fname_topo)
    lon_1d, lat_1d   = get_lonlat_1d(fname_topo)

    #lat-lon of the transect
    lon_tr, lat_tr = get_lonlat_trans(crd1, crd2, arlen)



    #get cartesian projections
    xx, yx = proj(lon_org, lat_org)
    xc_tr, yc_tr = proj(lon_tr, lat_tr)



    #get distance between vector points
    section_length = ((xc_tr.max() - xc_tr.min())**2 + (yc_tr.max() - yc_tr.min())**2)**.5
    l_tr = np.linspace(0, section_length, num=arlen)  # create a vector with distance between section points
    l_tr_2D = np.tile(l_tr, (data3D.shape[0], 1))  # create 2D array of shape (len(l_tr), data3D.shape[0]) out of this data

    # get bathymetry 2D array
    bathymetry_data = get_bathymetry_data(fname_topo)

    # create depth3D from layer height
    depth3D = get_depth3D(layerHeight3D, lon_1d, lat_1d, lon_tr, lat_tr)



    #interpolate the data...
    # -----------------------------------------------------------------------------------------------------------------
    # for every layer: interpolate data along the section line, interpolate the elevation data along the section line
    # then, form two 2D arrays for data and elevation-data with folowwing dimensions: (section_vector_lenght, number_of_z_layers)
    # then plot this two arrays
    # ADD: also interpolate bathymetry , and plot as line

    cs_bath = nn_interpolation(xx, yx, bathymetry_data, xc_tr, yc_tr, missing_value=-9999)  # cross-section bathymetry values



    interpolated_data = np.array([])
    interpolated_elev = np.array([])

    for layer_i in xrange(data3D.shape[0]):
        data_layer_i  = data3D[layer_i, ...]
        depth_layer_i = depth3D[layer_i, ...]

        cs_data_i = nn_interpolation(xx, yx, data_layer_i, xc_tr, yc_tr, missing_value=-9999)  # cross-section data at layer i
        cs_elev_i = nn_interpolation(xx, yx, depth_layer_i, xc_tr, yc_tr, missing_value=-9999)  # cross-section elevation at layer i

        # create 2D arrays of interpolated data and elevation
        if layer_i == 0:
            interpolated_data = cs_data_i
            interpolated_elev = cs_elev_i
        else:
            interpolated_data = np.vstack((interpolated_data, cs_data_i))
            interpolated_elev = np.vstack((interpolated_elev, cs_elev_i))
    # ----------------------------------------------------------------------------------------------------------------

    DATA_SECTION = dict()
    DATA_SECTION['x'] = l_tr_2D
    DATA_SECTION['y'] = interpolated_elev
    DATA_SECTION['v'] = interpolated_data
    DATA_SECTION['clim'] = clim
    DATA_SECTION['bath'] = cs_bath
    DATA_SECTION['label'] = titlestr

    #make plots
    makeplots(proj, xx, yx, get_bathymetry_data(fname_topo), xc_tr, yc_tr, DATA_SECTION, clim,  ax1=ax1, ax2=ax2)



def makeplots(proj, xc, yc, val_org, xc_tr, yc_tr, data_section, clim, ax1=None, ax2=None):
    print "making plots..."
    if ax1 is None:
        f = plt.figure(figsize=(8, 4), dpi=100, tight_layout=True)
        ax1=plt.subplot(1, 2, 1)
        ax2 = plt.subplot(1, 2, 2)
    #f.text(0.5, 0.96, titlestr, horizontalalignment='center')

    #2-D plot:
    plot2Dmap(ax1, clim, xc, yc, val_org, proj)  #background:
    proj.plot(xc_tr, yc_tr, ax=ax1)  #mark the transect:

    #2-D section-plot:
    plot2Dsection(data_section, ax=ax2)
    #f.show()


def plot2Dsection(data, ax=None):

    #data['y'] = np.flipud(data['y'])
    #data['v'] = np.flipud(data['v'])
    print "x shape:", data['x'].shape
    print "y shape:", data['y'].shape
    print "v shape:", data['v'].shape

    pcm = ax.pcolormesh(data['x'], data['y'], data['v'], vmin=data['v'].min(), vmax=data['v'].max())
    if 'bath' in data.keys():
        print data['x'][0, :].shape, data['bath'].shape
        ax.plot(data['x'][0, :], data['bath'], 'k')
    plt.colorbar(pcm, ax=ax, label=data['label'])
    ax.invert_yaxis()
    ax.set_ylabel("elevation [m BMSL]")

    scale = 0.001  #converting from [m] into [km]
    ticks = ticker.FuncFormatter(lambda x, pos: "{0:g}".format(x*scale))
    ax.xaxis.set_major_formatter(ticks)
    ax.set_xlabel("distance [km]")


def plot2Dmap(ax, clim, x, y, v0, proj):
    #print "x shape:", x.shape
    #print "y shape:", y.shape
    #print "v shape:", v0.shape

    pcf = proj.pcolormesh(x, y, v0, cmap=plt.get_cmap('rainbow_r'), vmin=v0.min(), vmax=v0.max(), ax=ax)

    #coastlines, etc
    proj.drawcoastlines(color=(0.3, 0.3, 0.3), linewidth=0.5, ax=ax)
    proj.drawmapboundary(fill_color=(0.9, 0.9, 1.0), ax=ax)
    proj.fillcontinents((1.0, 1.0, 1.0), lake_color=(0.9, 0.9, 1.0), ax=ax)

    cbar = proj.colorbar(pcf, location='bottom', pad='5%', label='Bathymetry: [m BMSL]')



def get_depth3D(layer_height_3D, lon_1D, lat_1D, newlon, newlat)  :
    # newlon, newlat, log=False):
    depth3D = np.empty((layer_height_3D.shape[0]-1, layer_height_3D.shape[1], layer_height_3D.shape[2]))

    #find indices to keep:
    dlon = lon_1D[1]-lon_1D[0]
    dlat = lat_1D[1]-lat_1D[0]

    keep_x = np.where((lon_1D >= newlon.min()-dlon) & (lon_1D <= newlon.max()+dlon))
    keep_y = np.where((lat_1D >= newlat.min()-dlat) & (lat_1D <= newlat.max()+dlat))

    for x in keep_x[0].tolist():
        for y in keep_y[0].tolist():
            for z in xrange(depth3D.shape[0]):
                depth3D[z, y, x] = (layer_height_3D[z, y, x]+layer_height_3D[z+1, y, x])/2.

    return depth3D




def getvar_ffile(f, varname):
    nc = netCDF4.Dataset(f)
    var = nc.variables[varname]
    try:
        titlestr = varname + ' ['+var.units+']'
    except:
        titlestr = varname
    return titlestr


def get_bathymetry_data(fname_topo):
    topo = get_getm_bathymetry(fname_topo)
    return topo['H']

def get_lonlat_org(fname_topo):
    topo = get_getm_bathymetry(fname_topo)
    return((topo['lons'], topo['lats']))

def get_lonlat_1d(fname):
    topo = get_getm_bathymetry(fname)
    return((topo['lons_1d'], topo['lats_1d']))


def get_getm_bathymetry(fname, bathymetry_varname='bathymetry'):
    ncB = netCDF4.Dataset(fname)
    ncBv = ncB.variables
    lat_name = ncBv[bathymetry_varname].dimensions[-2]  # str, name of the variable
    lon_name = ncBv[bathymetry_varname].dimensions[-1]  # str, name of the variable

    lons_1d = ncBv[lon_name][:]
    lats_1d = ncBv[lat_name][:]

    lons, lats = np.meshgrid(lons_1d, lats_1d)

    H = ncBv[bathymetry_varname][:, :]
    try:
        units = ncBv[bathymetry_varname].units
    except:
        units = "?"
    topo = {'H': H, 'lats': lats, 'lons': lons, 'Hunit': units, 'lats_1d':lats_1d, 'lons_1d':lons_1d}
    ncB.close()
    return(topo)



def get_projection_extent_from_bathymetry(fname, bathymetry_varname='bathymetry'):
    ncB = netCDF4.Dataset(fname)
    ncBv = ncB.variables
    lat_name = ncBv[bathymetry_varname].dimensions[-2]  # str, name of the variable
    lon_name = ncBv[bathymetry_varname].dimensions[-1]  # str, name of the variable

    lons_1d = ncBv[lon_name][:]
    lats_1d = ncBv[lat_name][:]

    DATA = dict()
    DATA["projection"] = "lcc"
    DATA["resolution"] = "h"
    DATA["llcrnrlon"] = lons_1d[0]
    DATA["llcrnrlat"] = lats_1d[0]
    DATA["urcrnrlon"] = lons_1d[-1]
    DATA["urcrnrlat"] = lats_1d[-1]
    DATA["lat_0"] = (lons_1d[-1] - lons_1d[0])*0.5
    DATA["lon_0"] = (lats_1d[-1] - lats_1d[0])*0.5

    ncB.close()
    return DATA


def get_proj(projpath, PRJ_DATA=None, bathymetry_fname=None):
    try:
        f=open(os.path.join(projpath, 'proj.pickle'), 'rb')
        (proj,)=pickle.load(f)
        f.close()
        print 'loaded '+projpath+'proj.pickle'
    except:
        #sns
        if PRJ_DATA:  # user defined projection... for future...
            print "creating basemap based on user input.."
            proj = Basemap(projection=PRJ_DATA['projection'], resolution=PRJ_DATA['resolution'], llcrnrlon=PRJ_DATA['llcrnrlon'],
                llcrnrlat=PRJ_DATA['llcrnrlat'], urcrnrlon=PRJ_DATA['urcrnrlon'], urcrnrlat=PRJ_DATA['urcrnrlat'], lat_0=PRJ_DATA['lat_0'],
                lon_0=PRJ_DATA['lon_0'])
        else:  # if not user defined projection....
            if bathymetry_fname:
                print "creating basemap based on bathymetry data..."
                PRJ_DATA = get_projection_extent_from_bathymetry(bathymetry_fname)  # overwriting non-exesting projection data with one generated from bathymetry
                proj = Basemap(projection=PRJ_DATA['projection'], resolution=PRJ_DATA['resolution'], llcrnrlon=PRJ_DATA['llcrnrlon'],
                llcrnrlat=PRJ_DATA['llcrnrlat'], urcrnrlon=PRJ_DATA['urcrnrlon'], urcrnrlat=PRJ_DATA['urcrnrlat'], lat_0=PRJ_DATA['lat_0'],
                lon_0=PRJ_DATA['lon_0'])

            else:
                print "creating basemap based on default settings..."
                proj = Basemap( projection='lcc',
                                   resolution='l',
                                   llcrnrlon=-0.8,
                                   llcrnrlat=50.7,
                                   urcrnrlon=9.7,
                                   urcrnrlat=55.7,
                                   lat_0=54.0,
                                   lon_0=6.)

        #pickle for later use:
        f=open(os.path.join(projpath, 'proj.pickle'), 'wb')
        pickle.dump((proj,), f)  #,protocol=-1
        f.close()
    return(proj)


def get_lonlat_trans(crd1, crd2, arlen):
    #import LatLon
    #ll1 = LatLon.LatLon(crd1[0], crd1[1])
    #ll2 = LatLon.LatLon(crd2[0], crd2[1])
    #stepvec = (ll2-ll1)/(arlen-1)
    #steps = [ll1+x*stepvec for x in range(0, arlen)]
    #lats = np.array([step.lat.decimal_degree for stepno, step in enumerate(steps)])
    #lons = np.array([step.lon.decimal_degree for stepno, step in enumerate(steps)])
    #print lats
    d_lat = crd2[0] - crd1[0]
    d_lon = crd2[1] - crd1[1]
    step_lat = d_lat/float(arlen-1)
    step_lon = d_lon/float(arlen-1)
    lats = np.array([crd1[0] + step_lat*x for x in xrange(arlen)])
    lons = np.array([crd1[1] + step_lon*x for x in xrange(arlen)])

    print 'comparing lats>>>', lats[-1], crd2[0]
    print 'comparing lons>>>', lons[-1], crd2[1]
    return(lons, lats)



def nn_interpolation(lon, lat, values, newlon, newlat, missing_value=-999):
    #take all as 1-D arrays
    lon1 = np.reshape(lon, lon.size)
    lat1 = np.reshape(lat, lon.size)
    val1 = np.reshape(values, lon.size)

    #find indices to keep:
    ikeep=np.where((lon1>=newlon.min()-0.5) & (lon1<=newlon.max()+0.5) &
                   (lat1>=newlat.min()-0.5) & (lat1<=newlat.max()+0.5))
    #trim the arrays
    lon1=lon1[ikeep]
    lat1=lat1[ikeep]
    val1=val1[ikeep]

    if val1.size==0:
        return

    coords = np.asarray(zip(lon1, lat1))
    f = interpolate.NearestNDInterpolator(coords, val1)
    #f = interpolate.LinearNDInterpolator(coords, asarray(valuelist),fill_value=missing_value)
    res = f(newlon, newlat)
    #for whatever the reason, for indices falling out the data coverage,
    #interpolation doesn't result in nan by -8.??e+38
    maskdef = np.zeros(res.shape, dtype=bool)
    if hasattr(res, 'mask'):
        maskdef=res.mask
    #res[where((maskdef) | (isnan(res)) | (res<0) | (extramask) )]=missing_value
    res[np.where((maskdef) | (np.isnan(res)) | (res<0) )] = missing_value
    return np.ma.masked_equal(res, missing_value)





def getvar_ffile_old(f, varname):
    nc=netCDF4.Dataset(f)
    var=nc.variables[varname]
    val_org = np.squeeze(var[:, :])
    nc.close()
    return val_org

if __name__=="__main__":




    crd1 = [55.8392981632, 3.11367348564]
    crd2 = [53.878255541, 8.86602775453]
    arlen = 100
    varname = 'bathymetry'
    fname      = '/home/uwhpsc/baw/014/014_arcMap_map_TF_into_MOSSCO_grid/DHI.600m.v22_tf.nc'
    fname_topo      = '/home/uwhpsc/baw/014/014_arcMap_map_TF_into_MOSSCO_grid/DHI.600m.v22_tf.nc'
    _2D_bathymetry = getvar_ffile_old(fname, varname)


    # artificial_data
    n_layers = 5  # assume we have 5 layers
    depth3D = np.tile(_2D_bathymetry, (n_layers, 1, 1))

    # artificial_data
    _2D_bathymetry = getvar_ffile_old(fname_topo, 'bathymetry')
    layerHeight3D = np.tile(_2D_bathymetry, (n_layers+1, 1, 1))
    for l in xrange(n_layers):
        layerHeight3D[l, ...] = layerHeight3D[l, ...]*(float(n_layers-l)/float(n_layers))
    layerHeight3D[-1, ...] = 0.

    data3D = np.random.uniform(low=0.0, high=100., size=(n_layers, _2D_bathymetry.shape[0], _2D_bathymetry.shape[1]))
    for i in range(n_layers):
        data3D[i, ...] = np.random.uniform(low=20*i, high=20*(i+1), size=(_2D_bathymetry.shape[0], _2D_bathymetry.shape[1]))


    main(crd1, crd2, fname, fname_topo, varname, data3D, layerHeight3D, arlen=arlen, PRJ_DATA=None, clim=[0, 100])

