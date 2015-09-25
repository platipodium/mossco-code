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


def main(crd1, crd2, fname, fname_topo, varname, val_org, arlen=100, PRJ_DATA=None, clim=[0, 100], ax1=None, ax2=None):
    '''define coordinates of 2 points defining a transect
    #crd1=[54.02,6.58]
    #crd2=[55.00,6.8]
    #arlen=100 #resulting array length

    fname      = '/home/ak2stud/Nick/python_scripts/dev/uGrid/data/NSBS/netcdf_reference_3d.nc'
    fname_topo = '/home/ak2stud/Nick/python_scripts/dev/uGrid/data/NSBS/topo.nc'
    varname    = 'concentration_of_SPM_in_water_001'
    tind = 1
    lind = 0
    '''

    fpath = os.path.dirname(fname)
    print "Running main()"
    titlestr = getvar_ffile(fname, varname)

    #get a projection
    proj = get_proj(fpath, PRJ_DATA=PRJ_DATA, bathymetry_fname=fname_topo)

    #original lat&lon (xc&yc) of the grid (centers)
    lon_org, lat_org = get_lonlat_org(fname_topo)

    #lat-lon of the transect
    lon_tr, lat_tr = get_lonlat_trans(crd1, crd2, arlen)

    #get cartesian projections
    xx, yx = proj(lon_org, lat_org)
    xc_tr, yc_tr = proj(lon_tr, lat_tr)

    #interpolate
    val_tr = nn_interpolation(xx, yx, val_org, xc_tr, yc_tr, missing_value=-9999)

    #make plots
    makeplots(fpath, proj, xx, yx, val_org, xc_tr, yc_tr, val_tr, titlestr, clim, ax1=ax1, ax2=ax2)

    return((xc_tr, yc_tr, val_tr))


def makeplots(fpath, proj, xc, yc, val_org, xc_tr, yc_tr, val_tr, titlestr, clim, ax1=None, ax2=None):
    print "making plots..."
    if ax1 is None:
        f = plt.figure(figsize=(8, 4), dpi=100)
        ax1=plt.subplot(1, 2, 1)
        ax2 = plt.subplot(1, 2, 2)
    ax1.set_title('bathymetry')
    ax2.set_title(titlestr)

    #2-D plot:
    plot2Dmap(ax1, clim, xc, yc, val_org, proj)  #background:
    proj.plot(xc_tr, yc_tr)  #mark the transect:

    #1-D plot:
    print 'val_org:', val_org
    ax2.plot(np.arange(0, len(val_tr)), val_tr)
    #f.savefig("figure.pdf")


def plot2Dmap(ax, clim, x, y, v0, proj):
    #print "x shape:", x.shape
    #print "y shape:", y.shape
    #print "v shape:", v.shape
    pcf = proj.pcolormesh(x, y, v0, cmap=plt.get_cmap('rainbow_r'), vmin=clim[0], vmax=clim[1], ax=ax)

    #coastlines, etc
    proj.drawcoastlines(color=(0.3, 0.3, 0.3), linewidth=0.5, ax=ax)
    proj.drawmapboundary(fill_color=(0.9, 0.9, 1.0), ax=ax)
    proj.fillcontinents((1.0, 1.0, 1.0), lake_color=(0.9, 0.9, 1.0), ax=ax)

    cbar = proj.colorbar(pcf, location='bottom', pad='5%')





def getvar_ffile_old(f, varname):
    nc=netCDF4.Dataset(f)
    var=nc.variables[varname]
    val_org = np.squeeze(var[:, :])
    nc.close()
    return val_org



def getvar_ffile(f, varname):
    nc = netCDF4.Dataset(f)
    var = nc.variables[varname]
    try:
        titlestr = var.long_name + ' ['+var.units+']'
    except:
        titlestr = varname
    return titlestr


def get_lonlat_org(fname_topo):
    topo = get_getm_bathymetry(fname_topo)
    return((topo['lons'], topo['lats']))


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
    topo = {'H': H, 'lats': lats, 'lons': lons, 'Hunit': units}
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
                proj=Basemap(projection='lcc',
                                   resolution='h',
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
    print f
    #f = interpolate.LinearNDInterpolator(coords, asarray(valuelist),fill_value=missing_value)
    res = f(newlon, newlat)
    print len(res)
    #for whatever the reason, for indices falling out the data coverage,
    #interpolation doesn't result in nan by -8.??e+38
    maskdef = np.zeros(res.shape, dtype=bool)
    if hasattr(res, 'mask'):
        maskdef=res.mask
    #res[where((maskdef) | (isnan(res)) | (res<0) | (extramask) )]=missing_value
    res[np.where((maskdef) | (np.isnan(res)) | (res<0) )] = missing_value
    return np.ma.masked_equal(res, missing_value)


if __name__=="__main__":


    crd1 = [53.9828, 4.8882]
    crd2 = [55.9247, 3.2062]
    arlen = 100
    varname = 'bathymetry'
    fname_topo = '/home/uwhpsc/baw/014/014_arcMap_map_TF_into_MOSSCO_grid/DHI.600m.v22_tf.nc'
    fname      = '/home/uwhpsc/baw/014/014_arcMap_map_TF_into_MOSSCO_grid/DHI.600m.v22_tf.nc'
    val_org = getvar_ffile_old(fname, varname)

    main(crd1, crd2, fname, fname_topo, varname, val_org, arlen=100)
