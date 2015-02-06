#! /usr/bin/env python
# -*- coding: utf-8 -*-
# This python script is part of MOSSCO
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
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
import scipy.interpolate

def read_topo_ncdf(ncfile):
  nc = netCDF4.Dataset(ncfile)
  ncv = nc.variables
  z = np.squeeze(ncv['bathymetry'][:])  
  lon = np.squeeze(ncv['lon'][:])  
  lat = np.squeeze(ncv['lat'][:])  

  nc.close()

  return lon,lat,z

def write_topo_ncdf(filename,lon,lat,value):
    
  nc=netCDF4.Dataset(filename,'w',format='NETCDF3_CLASSIC')

  nx=len(lon)
  ny=len(lat)
  
  nc.createDimension('lon',nx)
# nc.createDimension('lon_x',nx+1)
  nc.createDimension('lat',ny)
#  nc.createDimension('lat_x',ny+1)
 
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
  var.missing_value = -10.0
  var[:,:] = value
  
  nc.type = "GETM topo file" ;
  nc.history = "Created by topo2topo.py"

  nc.close()

if __name__ == '__main__':

  basename = '/Users/lemmen/devel/MOSSCO/setups/deep_lake/topo.nc'
  lon, lat, z = read_topo_ncdf(basename)
  lonx, latx = np.meshgrid(lon, lat)
  
  nlon = len(lon)
  nlat = len(lat)
  dlon=lon[2]-lon[1]
  dlat=lat[2]-lat[1]
  mlon=np.mean(lon)
  mlat=np.mean(lat)
    
  for i in range(0,20):
      
    ny=nlat*(i+1)
    nx=nlon*(i+1)
    
    loni=np.arange(start=0, stop=nx)
    loni=(loni-nx/2.0+0.5)*dlon + mlon
    
    lati=np.arange(start=0, stop=ny)
    lati=(lati-ny/2.0+0.5)*dlat + mlat

    lony, laty = np.meshgrid(loni, lati)  
    lon=np.linspace(start=loni[0], stop=loni[-1], num=nlon)
    lat=np.linspace(start=lati[0], stop=lati[-1], num=nlat)
     
    f=scipy.interpolate.interp2d(lon, lat, z, kind='linear')
    zx = f(loni,lati)
  
    namei=basename.replace('.nc','_' + str(nx) + 'x' + str(ny) + '.nc')
    write_topo_ncdf(namei,loni, lati, zx)



