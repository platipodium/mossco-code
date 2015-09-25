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

def read_poro_ncdf(ncfile):
  nc = netCDF4.Dataset(ncfile)
  ncv = nc.variables
  z = np.squeeze(ncv['porosity_at_soil_surface'][:])  
  lon = np.squeeze(ncv['lon'][:])  
  lat = np.squeeze(ncv['lat'][:])  

  nc.close()

  return lon,lat,z

def write_poro_ncdf(filename,lon,lat,value):
    
  nc=netCDF4.Dataset(filename,'w',format='NETCDF3_CLASSIC')

  nx=len(lon)
  ny=len(lat)
  
  nc.createDimension('lon',nx)
  nc.createDimension('lat',ny)
  nc.createDimension('time',None)
 
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
  
  var=nc.createVariable('getmGrid2D_getm_lon','f8',('lon'))
  var.long_name = 'Longitude'
  var.units = 'degree_east'  
  var[:] = lon 

  var=nc.createVariable('getmGrid2D_getm_lat','f8',('lat'))
  var.long_name = 'Latitude'
  var.units = 'degree_north'
  var[:] = lat

  var=nc.createVariable('time','f8',('time'))
  var.long_name = 'time'
  var.units = 'seconds since 2000-01-01 00:00:00'
  var[:] = 0

  var=nc.createVariable('porosity_at_soil_surface','f8',('time','lat','lon'))
  var.long_name = "Porosity"
  var.units = "m^3/m^3" 
  var.valid_min = 0.3
  var.valid_max = 1.0
  var.missing_value = -1.0
  
  #value3=np.zeros((1,value.shape[0],value.shape[1]))
  #value3[]]
  var[0,:,:] = value
  
  nc.type = "MOSSCO porosity file" ;
  nc.history = "Created by poro2poro.py"

  nc.close()

if __name__ == '__main__':

  file1='/Users/lemmen/devel/mossco/setups/NSBS6nm/Forcing/porosity/porosity_NSBS3nm_zwy.nc'
  lon1, lat1, p1 = read_poro_ncdf(file1)

  # Extend data beyond edges
  for ilon in range(1,len(lon1-1)):
    for ilat in range(1,len(lat1-1)):
      p=p1[ilat-1:ilat+2,ilon-1:ilon+2]
      if np.max(p)>0 and np.min(p)<0:
        p[p<0]=np.mean(p[p>0])    
        p1[ilat-1:ilat+2,ilon-1:ilon+2]=p

  p1[p1<0.3]=0.3

  file2='/Users/lemmen/devel/mossco/setups/NSBS6nm/Forcing/porosity/porosity_NSBS6nm_v3.nc'
  lon2, lat2, p2 = read_poro_ncdf(file2)
  
  f=scipy.interpolate.interp2d(lon1, lat1, p1, kind='linear')
  zx = f(lon2,lat2)
  zx[zx<0]=1.0
  zx[zx>1]=1.0
  zx[p2<0]=-1.0 
  
  namei=file1.replace('.nc','_interpolated.nc')
  write_poro_ncdf(namei,lon2, lat2, zx)



