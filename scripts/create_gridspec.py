# -*- coding: utf-8 -*-
"""
# This script is is part of MOSSCO. It creates from basic grid parameters
# a GRIDSPEC compliant NetCDF file.
#
# @copyright (C) 2014, 2015, 2016 ,2017 Helmholtz-Zentrum Geesthacht
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

if (1==1):
  ll_lon = -4.		#lower left of cell corner 4°W
  ll_lat = 50.
  ur_lon = 15.
  ur_lat = 61.
  delta_lon = 0.035		#delta lon in dezimalgrad
  delta_lat = 0.02
if (2==2): ## spherical box / deep lake test case
  delta_lon=0.01250
  delta_lat=0.25/30.0
  ll_lon=0.0
  ur_lon=1.25
  ll_lat=45.0
  ur_lat=45.25
if (3==3): ## 1x12 box, mussel experiment
  delta_lon=0.5
  delta_lat=1
  ll_lon=0.0
  ur_lon=10.0
  ll_lat=54
  ur_lat=55

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
    basename = 'gridspec_example.nc'

  nc=netCDF4.Dataset(basename,'w',format='NETCDF3_CLASSIC')

  nc.createDimension('bound',2)
  nc.createDimension('lon',nlon)
  nc.createDimension('lat',nlat)

  lon = nc.createVariable('lon','f8',('lon'))
  lon.bounds='lon_bnds'
  lon.units='degree_east'
  lon.long_name='longitude'
  lon.standard_name='longitude'

  lon_bnds = nc.createVariable('lon_bnds','f8',('lon','bound'))

  lat = nc.createVariable('lat','f8',('lat'))
  lat.bounds='lat_bnds'
  lat.units='degree_north'
  lat.long_name='latitude'
  lat.standard_name='latitude'

  lat_bnds = nc.createVariable('lat_bnds','f8',('lat','bound'))

# Meta data
  nc.history = 'Created ' + time.ctime(time.time()) + ' by ' + sys.argv[0]
  nc.creator = 'Carsten Lemmen <carsten.lemmen@hzg.de>'
  nc.license = 'Creative Commons share-alike (CCSA)'
  nc.copyright = 'Helmholtz-Zentrum Geesthacht'
  nc.Conventions = 'CF-1.6'

  ilon=numpy.array(range(0,nlon))
  jlat=numpy.array(range(0,nlat))

  lon[:]=ll_lon+(ilon+0.5)*delta_lon
  lat[:]=ll_lat+(jlat+0.5)*delta_lat
  lon_bnds[:,0]=lon[:]-0.5*delta_lon
  lon_bnds[:,1]=lon[:]+0.5*delta_lon
  lat_bnds[:,0]=lat[:]-0.5*delta_lat
  lat_bnds[:,1]=lat[:]+0.5*delta_lat

  print lon_bnds[0,:], lat_bnds[-1,:]

  nc.close()
