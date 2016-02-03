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

  basename = '/Users/lemmen/temp/topo.nc'

  nc = netCDF4.Dataset(basename,'r')
  ncv = nc.variables
  z = np.squeeze(ncv['bathymetry'][:])  
  lonx = np.squeeze(ncv['lonx'][:])  
  latx = np.squeeze(ncv['latx'][:]) 
#  convx = squeeze(ncv['convx'][:]) 
#  xx = squeeze(ncv['xx'][:]) 
#  yx = squeeze(ncv['yx'][:]) 
  missing_value=ncv['bathymetry'].missing_value

  nc.close()

# Latx/Lonx are the corner locations
# Find the center location as the geometric center

  lat=latx[:-1,:-1] + 0.5*(latx[1:,1:]-latx[:-1,:-1])
  lon=lonx[:-1,:-1] + 0.5*(lonx[1:,1:]-lonx[:-1,:-1])

  nx=np.shape(lon)[0]
  ny=np.shape(lon)[1]


  mask=np.array((nx,ny),dtype=int)
  #mask[z==missing_value]=0

  ncfile=re.sub('.nc','',basename) + '_scrip.nc'
  
  nc=netCDF4.Dataset(ncfile,'w',format='NETCDF3_CLASSIC')
 
  nc.createDimension('grid_size',nx*ny)
  nc.createDimension('grid_corners',4)
  nc.createDimension('grid_rank',2)
 
 
  grid_dims = nc.createVariable('grid_dims','i4',('grid_rank'))
  grid_dims.units = 'unitless'

  grid_imask = nc.createVariable('grid_imask','i4',('grid_size'))
  grid_imask.units = 'unitless'

  grid_center_lat = nc.createVariable('grid_center_lat','f8',('grid_size'))
  grid_center_lat.units='degree_north' 

  grid_center_lon = nc.createVariable('grid_center_lon','f8',('grid_size'))
  grid_center_lon.units='degree_east' 

  grid_corner_lat = nc.createVariable('grid_corner_lat','f8',('grid_size','grid_corners'))
  grid_corner_lat.units='degree_north' 
 
  grid_corner_lon = nc.createVariable('grid_corner_lon','f8',('grid_size','grid_corners'))
  grid_corner_lon.units='degree_east' 

 
# Meta data
  nc.history = 'Created ' + time.ctime(time.time()) + ' by ' + sys.argv[0]
  nc.creator = 'Carsten Lemmen <carsten.lemmen@hzg.de>'
  nc.license = 'Creative Commons share-alike (CCSA)'
  nc.copyright = 'Helmholtz-Zentrum Geesthacht'
  nc.Conventions = 'SCRIP'

# Values
  grid_dims[:]=[nx,ny]
  grid_imask[:]=1

  grid_center_lat[:]=np.reshape(lat, nx*ny)
  grid_center_lon[:]=np.reshape(lon, nx*ny)

# Corners from ll anticlockwise to ul
  grid_corner_lat[:,0]=np.reshape(latx[0:-1,0:-1], nx*ny)
  grid_corner_lon[:,0]=np.reshape(lonx[0:-1,0:-1], nx*ny)
  grid_corner_lat[:,1]=np.reshape(latx[1:,0:-1], nx*ny)
  grid_corner_lon[:,1]=np.reshape(lonx[1:,0:-1], nx*ny)
  grid_corner_lat[:,2]=np.reshape(latx[1:,1:], nx*ny)
  grid_corner_lon[:,2]=np.reshape(lonx[1:,1:], nx*ny)
  grid_corner_lat[:,0]=np.reshape(latx[0:-1,1:], nx*ny)
  grid_corner_lon[:,0]=np.reshape(lonx[0:-1,1:], nx*ny)


  nc.close()

"""
  


  ilon=numpy.array(range(0,nlon))
  jlat=numpy.array(range(0,nlat))

  glon=ll_lon+(ilon+0.5)*delta_lon
  glat=ll_lat+(jlat+0.5)*delta_lat
  
  for j in jlat:
      k=ilon+j*nlon    
      grid_center_lon[k]=glon
      #grid_center_lat[k]=ll_lat + (repeat(j,nlon)+0.5) * delta_lat
  for i in ilon:
      k=jlat+i*nlat    
      grid_center_lat[k]=glat
      #grid_center_lat[k]=ll_lat + (repeat(j,nlon)+0.5) * delta_lat
     
           
  grid_corner_lon[:,0]=grid_center_lon[:] - 0.5 * delta_lon
  grid_corner_lon[:,1]=grid_center_lon[:] + 0.5 * delta_lon

  grid_corner_lat[:,0]=grid_center_lat[:]-0.5*delta_lat
  grid_corner_lat[:,2]=grid_center_lat[:]+0.5*delta_lat
  grid_corner_lon[:,2]=grid_corner_lon[:,1]
  grid_corner_lon[:,3]=grid_corner_lon[:,0]
  grid_corner_lat[:,1]=grid_corner_lat[:,0]
  grid_corner_lat[:,3]=grid_corner_lat[:,2]  
  
  nc.close()

"""
