#! /usr/bin/env python
# -*- coding: utf-8 -*-
# This python script is part of MOSSCO
#
# @copyright (C) 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

#from pylab import *
import netCDF4
import numpy as np

def write_topo_ncdf(filename,lon,lat,value):

  nc=netCDF4.Dataset(filename,'w',format='NETCDF3_CLASSIC')

  nx=len(lon)
  ny=len(lat)

  nc.createDimension('lon',nx)
  nc.createDimension('lat',ny)

  var=nc.createVariable('grid_type','i4',())
  var.long_name="Type of horizontal grid"
  var.option_1_ = "Cartesian"
  var.option_2_ = "Spherical"
  var.option_3_ = "Curvilinear"
  var.option_4_ = "Spherical Curvilinear"
  var[:] = 2

  var=nc.createVariable('rearth','f8',())
  var.long_name = "Radius of spherical Earth for mapping"
  var.units = "m"
  var[:] = 6378815.0

  var=nc.createVariable('lon','f8',('lon'))
  var.long_name = 'Longitude'
  var.units = 'degree_east'
  var[:] = lon

  var=nc.createVariable('lat','f8',('lat'))
  var.long_name = 'Latitude'
  var.units = 'degree_north'
  var[:] = lat

  var=nc.createVariable('bathymetry','f8',('lat','lon'))
  var.long_name = "Final bathymetry at T-points"
  var.units = "m"
  var.valid_min = 0.0
  var.valid_max = 100.0
  var.missing_value = -10
  var[:,:] = value[:,:]

  nc.type = "GETM topo file" ;
  nc.history = "Created by create_lake_topo.py"
  nc.project = "MOSSCO"

  nc.close()

if __name__ == '__main__':

  resolution = 120.0  # 120 points per degree = half arcsecond resolution
  lat0 = 50.0         # mean latitude
  n = np.arange(10,40)   # domain size
  z0 = 50.0           # Water depth
  missing = -10.0

  basename = '/Users/lemmen/devel/MOSSCO/setups/deep_lake/topo.nc'

  for i in n:
    nx=i
    lon=np.arange(start=0, stop=nx)
    lon=(lon - (nx-1)/2.0) / resolution

    for j in range(i, i+1):

      ny=j
      lat=np.arange(start=0, stop=ny)
      lat=(lat - (ny-1)/2.0) / resolution + lat0

      z=np.ndarray((nx,ny))*0.0 + z0
      z[0,: ]=-10
      z[-1,:]=-10
      z[:,0 ]=-10
      z[:,-1]=-10

      name=basename.replace('.nc','_' + str(int(z0)) + '_' + str(int(i*j)) + '_' + str(nx) + 'x' + str(ny) + '.nc')
      write_topo_ncdf(name,lon, lat, z)
