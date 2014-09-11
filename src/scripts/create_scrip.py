# -*- coding: utf-8 -*-
"""
# This script is is part of MOSSCO. It creates from basic grid parameters 
# a SCRIP complient NetCDF file.
#
# @copyright (C) 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
"""

import netCDF4
import sys
import numpy
import time

ll_lon = -4.		#lower left of cell corner 4°W
ll_lat = 50.
ur_lon = 15.
ur_lat = 61.
delta_lon = 0.0176		#delta lon in dezimalgrad
delta_lat = 0.02

       
if __name__ == '__main__':

  nlat=abs(ur_lat-ll_lat)/abs(delta_lat)
  nlon=abs(ur_lon-ll_lon)/abs(delta_lon)

  # ensure that nlat and nlot are whole numbers and adust delta
  if (nlat != round(nlat)):
      delta_lat=abs(ur_lat-ll_lat)/round(nlat)
  nlat=int(round(nlat))
   
  if (nlon != round(nlon)):
      delta_lon=abs(ur_lon-ll_lon)/round(nlon)
  nlon=int(round(nlon))

  if len(sys.argv)>1:
    basename=sys.argv[1]
  else:
    basename = 'create_scrip_example.nc'


  nc=netCDF4.Dataset(basename,'w',format='NETCDF3_CLASSIC')

  nc.createDimension('grid_size',nlon*nlat)
  nc.createDimension('grid_corners',4)
  nc.createDimension('grid_rank',2)
 
  grid_dims = nc.createVariable('grid_dims','i4',('grid_rank'))
  grid_imask = nc.createVariable('grid_imask','i4',('grid_size'))

  grid_center_lat = nc.createVariable('grid_center_lat','f8',('grid_size'))
  grid_center_lat.units='degree_north' 
#  grid_center_lat[:]= ...

  grid_center_lon = nc.createVariable('grid_center_lon','f8',('grid_size'))
  grid_center_lon.units='degree_east' 
#  grid_center_lon[:]= ...

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
  

  grid_dims[:]=[nlon,nlat]
  grid_imask[:]=1

  ilon=numpy.array(range(0,nlon))
  jlat=numpy.array(range(0,nlat))

  glon=ll_lon+((ilon+0.5)*(ur_lon-ll_lon))/nlon
  glat=ll_lat+((jlat+0.5)*(ur_lat-ll_lat))/nlat
  
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


