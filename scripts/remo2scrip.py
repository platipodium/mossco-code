# -*- coding: utf-8 -*-
"""
Created on Fri Jun 20 12:28:35 2014

@author: lemmen
"""
import netCDF4
import sys
import numpy as np
import time
import re


if __name__ == '__main__':

  basename = '/Volumes/Kea/data/remo/REMO-200303.nc'

  nc = netCDF4.Dataset(basename,'r')
  ncv = nc.variables
  lon = np.squeeze(ncv['lon'][:])
  lat = np.squeeze(ncv['lat'][:])
  
  nc.close()

# Lat/Lon are the center locations
# Find the corner location as the half distance

  nx=lon.size
  ny=lat.size

  dlat = lat[1:]-lat[:-1]
  dlon = lon[1:]-lon[:-1]

  latx = np.zeros((ny+1,))  
  lonx = np.zeros((nx+1,))  

  latx[0]    = lat[0]-0.5*dlat[0]
  latx[1:-1] = lat[:-1]+dlat[:]
  latx[-1]   = lat[-1]+dlat[-1]

  lonx[0]    = lon[0]-0.5*dlon[0]
  lonx[1:-1] = lon[:-1]+dlon[:]
  lonx[-1]   = lon[-1]+dlon[-1]

  xm, ym = np.meshgrid(lon, lat)
  xc, yc = np.meshgrid(lonx, latx)

  mask=np.array((xm>-1000), dtype=int)
  
  ncfile=re.sub('.nc','',basename) + '_scrip.nc'

  nc=netCDF4.Dataset(ncfile,'w',format='NETCDF3_CLASSIC')

  nc.createDimension('grid_size',nx*ny)
  nc.createDimension('grid_corners',4)
  nc.createDimension('grid_rank',2)

  grid_dims = nc.createVariable('grid_dims','i4',('grid_rank'))
  grid_dims.units = '1'

  grid_imask = nc.createVariable('grid_imask','i4',('grid_size'))
  grid_imask.units = '1'

  grid_center_lat = nc.createVariable('grid_center_lat','f8',('grid_size'))
  grid_center_lat.units='degrees'

  grid_center_lon = nc.createVariable('grid_center_lon','f8',('grid_size'))
  grid_center_lon.units='degrees'

  grid_corner_lat = nc.createVariable('grid_corner_lat','f8',('grid_size','grid_corners'))
  grid_corner_lat.units='degrees'

  grid_corner_lon = nc.createVariable('grid_corner_lon','f8',('grid_size','grid_corners'))
  grid_corner_lon.units='degrees'

# Meta data
  nc.history = 'Created ' + time.ctime(time.time()) + ' by ' + sys.argv[0]
  nc.creator = 'Carsten Lemmen <carsten.lemmen@hzg.de>'
  nc.license = 'Creative Commons Attribution Share-alike (CC-BY-SA)'
  nc.copyright = 'Helmholtz-Zentrum Geesthacht'
  nc.Conventions = 'SCRIP'

# Values
  grid_dims[:]=[nx,ny]
  grid_imask[:]=np.reshape(mask, nx*ny,  order='F')

  grid_center_lat[:]=np.reshape(ym, nx*ny, order='F')
  grid_center_lon[:]=np.reshape(xm, nx*ny, order='F')

# Corners from ll anticlockwise to ul
  grid_corner_lat[:,0]=np.reshape(yc[0:-1,0:-1], nx*ny, order='F')
  grid_corner_lon[:,0]=np.reshape(xc[0:-1,0:-1], nx*ny, order='F')
  grid_corner_lat[:,1]=np.reshape(yc[1:,0:-1], nx*ny, order='F')
  grid_corner_lon[:,1]=np.reshape(xc[1:,0:-1], nx*ny, order='F')
  grid_corner_lat[:,2]=np.reshape(yc[1:,1:], nx*ny, order='F')
  grid_corner_lon[:,2]=np.reshape(xc[1:,1:], nx*ny, order='F')
  grid_corner_lat[:,0]=np.reshape(yc[0:-1,1:], nx*ny, order='F')
  grid_corner_lon[:,0]=np.reshape(xc[0:-1,1:], nx*ny, order='F')

  nc.close()
