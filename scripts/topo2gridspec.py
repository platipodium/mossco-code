# -*- coding: utf-8 -*-
"""
# This script is is part of MOSSCO. It creates from a getm bathymetry/topography
# a GRIDSPEC compliant NetCDF file.
#
# @copyright (C) 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
"""
import netCDF4
import sys, os
import numpy as np
import time
import re

if __name__ == '__main__':

  basename = os.path.join(os.environ['MOSSCO_SETUPDIR'],'sns','Topo','topo.nc') 

  nc = netCDF4.Dataset(basename,'r')
  ncv = nc.variables
  z = np.squeeze(ncv['bathymetry'][:])
  lonx = np.squeeze(ncv['lonx'][:])
  latx = np.squeeze(ncv['latx'][:])
  
  nc.close()

# Latx/Lonx are the corner locations
# Find the center location as the geometric center

  #latx=np.transpose(latx)
  #lonx=np.transpose(lonx)
  #z=np.transpose(z)

  lat=latx[:-1,:-1] + 0.5*(latx[1:,1:]-latx[:-1,:-1])
  lon=lonx[:-1,:-1] + 0.5*(lonx[1:,1:]-lonx[:-1,:-1])

  nx=np.shape(lon)[1]
  ny=np.shape(lon)[0]
  nbound = 4 

  mask=np.array(np.logical_not(z.mask), dtype=int)
  
  ncfile=re.sub('.nc','',basename) + '_gridspec.nc'

  nc=netCDF4.Dataset(ncfile,'w',format='NETCDF3_CLASSIC',clobber=True)

  nc.createDimension('bounds',nbound)
  nc.createDimension('y',ny)
  nc.createDimension('x',nx)
  
  # This is a CF auxiliary coordinate
  lat_var = nc.createVariable('lat','f8',('y','x'))
  lat_var.units='degrees_north'
  lat_var.bounds='lat_bounds'
  
  lon_var = nc.createVariable('lon','f8',('y','x'))
  lon_var.units='degrees_east'
  lon_var.bounds='lon_bounds'

  # The bounds variable has no attribures (inherited from main var)
  lon_bounds_var = nc.createVariable('lon_bounds','f8',('bounds','y','x'))
  lat_bounds_var = nc.createVariable('lat_bounds','f8',('bounds','y','x'))

  # Add mask 
  mask_var = nc.createVariable('mask','i4',('y','x'))
  mask_var.missing_value=0
  mask_var.coordinates='lon lat'
  mask_var.units=''

  # Add mask 
  z_var = nc.createVariable('bathymetry','f4',('y','x'))
  z_var.missing_value=-1.0E30
  z_var.coordinates='lon lat'
  z_var.units='m'

# Meta data
  nc.history = 'Created ' + time.ctime(time.time()) + ' by ' + sys.argv[0]
  nc.creator = 'Carsten Lemmen <carsten.lemmen@hzg.de>'
  nc.license = 'Creative Commons Attribution Share-alike (CC-BY-SA)'
  nc.copyright = 'Helmholtz-Zentrum Geesthacht'
  nc.Conventions = 'CF-1.7'

  lat_var[:]=lat
  lon_var[:]=lon
  lat_bounds_var[0,:,:]=latx[:-1,:-1] # ll
  lat_bounds_var[1,:,:]=latx[ 1:,:-1] # lr
  lat_bounds_var[2,:,:]=latx[ 1:, 1:] # ur
  lat_bounds_var[3,:,:]=latx[:-1, 1:] # ul
  lon_bounds_var[0,:,:]=lonx[:-1,:-1] # ll
  lon_bounds_var[1,:,:]=lonx[ 1:,:-1] # lr
  lon_bounds_var[2,:,:]=lonx[ 1:, 1:] # ur
  lon_bounds_var[3,:,:]=lonx[:-1, 1:] # ul
  
  mask_var[:]=mask
  z[z<0]=z_var.missing_value
  z_var[:]=z

  nc.close()
