#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Script to convert a netcdf file with one-dimensional lon and lat coordinate
# variable to a SCRIP file. This  script is part of MOSSCO
#
# @copyright (C) 2016,2017 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file"""

import netCDF4
import sys
import numpy as np
import time
import re


def convert2scrip(basename):

    nc = netCDF4.Dataset(basename, 'r')
    ncv = nc.variables
    lon = np.squeeze(ncv['lon'][:])
    lat = np.squeeze(ncv['lat'][:])

    nc.close()

# Lat/Lon are the center locations
# Find the corner location as the half distance

    nx = lon.size
    ny = lat.size

    dlat = lat[1:] - lat[:-1]
    dlon = lon[1:] - lon[:-1]

    latx = np.zeros((ny + 1,))
    lonx = np.zeros((nx + 1,))

    latx[0] = lat[0] - 0.5 * dlat[0]
    latx[1:-1] = lat[:-1] + dlat[:]
    latx[-1] = lat[-1] + dlat[-1]

    lonx[0] = lon[0] - 0.5 * dlon[0]
    lonx[1:-1] = lon[:-1] + dlon[:]
    lonx[-1] = lon[-1] + dlon[-1]

    xm, ym = np.meshgrid(lon, lat)
    xc, yc = np.meshgrid(lonx, latx)

    mask = np.array((xm > -1000), dtype=int)

    ncfile = re.sub('.nc', '', basename) + '_scrip.nc'

    nc = netCDF4.Dataset(ncfile, 'w', format='NETCDF3_CLASSIC')

    nc.createDimension('grid_size', nx * ny)
    nc.createDimension('grid_corners', 4)
    nc.createDimension('grid_rank', 2)

    grid_dims = nc.createVariable('grid_dims', 'i4', ('grid_rank'))
    grid_dims.units = '1'

    grid_imask = nc.createVariable('grid_imask', 'i4', ('grid_size'))
    grid_imask.units = '1'

    grid_center_lat = nc.createVariable('grid_center_lat', 'f8', ('grid_size'))
    grid_center_lat.units = 'degrees'

    grid_center_lon = nc.createVariable('grid_center_lon', 'f8', ('grid_size'))
    grid_center_lon.units = 'degrees'

    grid_corner_lat = nc.createVariable(
        'grid_corner_lat', 'f8', ('grid_size', 'grid_corners'))
    grid_corner_lat.units = 'degrees'

    grid_corner_lon = nc.createVariable(
        'grid_corner_lon', 'f8', ('grid_size', 'grid_corners'))
    grid_corner_lon.units = 'degrees'

# Meta data
    nc.history = 'Created ' + time.ctime(time.time()) + ' by ' + sys.argv[0]
    nc.creator = 'Carsten Lemmen <carsten.lemmen@hzg.de>'
    nc.license = 'Creative Commons Attribution Share-alike (CC-BY-SA)'
    nc.copyright = 'Helmholtz-Zentrum Geesthacht'
    nc.Conventions = 'SCRIP'

# Values
    grid_dims[:] = [nx, ny]
    grid_imask[:] = np.reshape(mask, nx * ny,  order='F')

    grid_center_lat[:] = np.reshape(ym, nx * ny, order='F')
    grid_center_lon[:] = np.reshape(xm, nx * ny, order='F')

# Corners from ll anticlockwise to ul
    grid_corner_lat[:, 0] = np.reshape(yc[0:-1, 0:-1], nx * ny, order='F')
    grid_corner_lon[:, 0] = np.reshape(xc[0:-1, 0:-1], nx * ny, order='F')
    grid_corner_lat[:, 1] = np.reshape(yc[1:, 0:-1], nx * ny, order='F')
    grid_corner_lon[:, 1] = np.reshape(xc[1:, 0:-1], nx * ny, order='F')
    grid_corner_lat[:, 2] = np.reshape(yc[1:, 1:], nx * ny, order='F')
    grid_corner_lon[:, 2] = np.reshape(xc[1:, 1:], nx * ny, order='F')
    grid_corner_lat[:, 0] = np.reshape(yc[0:-1, 1:], nx * ny, order='F')
    grid_corner_lon[:, 0] = np.reshape(xc[0:-1, 1:], nx * ny, order='F')

    nc.close()

def add2scrip(basename,variables):

    nc = netCDF4.Dataset(basename, 'r')
    ncv = nc.variables
    lon = np.squeeze(ncv['lon'][:])
    lat = np.squeeze(ncv['lat'][:])

    nx = lon.size
    ny = lat.size

    ncfile = re.sub('.nc', '', basename) + '_scrip.nc'
    ncout = netCDF4.Dataset(ncfile, 'a', format='NETCDF3_CLASSIC')

    for v in variables.split(','):

        if v not in ncv: continue
        if v in ncout.variables.keys(): continue

        print '  Adding variable '

        if '_FillValue' in ncv[v].ncattrs(): fillValue = ncv[v]._FillValue
        else: fillValue =1E20

        var = ncout.createVariable(v,'f4',('grid_size'), fill_value=fillValue)

        for att in ncv[v].ncattrs():
            var.setncattr(att,ncv[v].getncattr(att))

        var[:] = np.reshape(ncv[v][:], nx * ny,  order='F')

    ncout.close()
    nc.close()


if __name__ == '__main__':

    if (len(sys.argv) > 1):
        basename = sys.argv[1]
    else:
        basename = '/Volumes/Kea/data/remo/REMO-200303.nc'

    convert2scrip(basename)

    # There should not be any user data in a SCRIP file, so this is commented out
    #if (len(sys.argv) > 2):
    #    basename = sys.argv[1]
    #    variables=sys.argv[2]
    #    add2scrip(basename, variables)
