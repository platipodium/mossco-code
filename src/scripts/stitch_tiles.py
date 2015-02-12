#!/usr/bin/env python
#> @brief Stitching script for multiprocessor tiled output of getm/netcdf components

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2015 Helmholtz Zentrum Geesthacht
#> @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

import netCDF4 as netcdf
import glob as glob
import numpy as np

# Adjust the file name here (or improve the script to handle cmdline args
files=glob.glob("/Volumes/Kiwi/mossco/output/mossco_gffn_out.??.surface.nc")

ulon=[]
ulat=[]
for f in files:
  nc=netcdf.Dataset(f,'r')
  lat=nc.variables['getmGrid2D_getm_lat'][:]
  lon=nc.variables['getmGrid2D_getm_lon'][:]
  time=nc.variables['time'][:]
  ulon.extend(lon)
  ulat.extend(lat)
  nc.close()

nc=netcdf.Dataset(files[0],'r')
ulon=set(ulon)
ulat=set(ulat)

dimDict={'getmGrid2D_getm_1':'lon','getmGrid2D_getm_2':'lat','getmGrid3D_getm_1':'lon',
         'getmGrid3D_getm_2':'lat','getmGrid3D_getm_3':'height','ungridded00024':'depth','time':'time'}

varDict={'getmGrid2D_getm_lon':'lon','getmGrid2D_getm_lat':'lat','getmGrid3D_getm_lon':'lon',
         'getmGrid3D_getm_lat':'lat','getmGrid3D_getm_layer':'height'}

ncout = netcdf.Dataset('tile.nc', 'w', format='NETCDF4_CLASSIC')
ncout.createDimension('time',len(time))
ncout.createDimension('lat',len(ulat))
ncout.createDimension('lon',len(ulon))

for item in ['getmGrid3D_getm_3','ungridded00024']:
  if nc.variables.has_key(item):
    ncout.createDimension(dimDict[item],len(nc.dimensions[item]))

for key,value in nc.variables.iteritems():
  dims=list(value.dimensions)
  if varDict.has_key(key): key=varDict[key]
  for i in range(0,len(dims)): dims[i]=dimDict[dims[i]]
  try:
    var=ncout.createVariable(key,value.dtype,tuple(dims))
    for att in value.ncattrs():
      if (att != 'coordinates'):
        var.setncattr(att,value.getncattr(att))
  except:
    print 'skipped ' + key

ulon=np.sort(list(ulon))
ulat=np.sort(list(ulat))

ncout.variables['lon'][:]=ulon
ncout.variables['lat'][:]=ulat
ncout.variables['time'][:]=time
#ncout.variables['height'][:]=nc.variables['getmGrid3D_getm_layer'][:]
nc.close()

for f in files[:]:
  nc=netcdf.Dataset(f,'r')
  lat=nc.variables['getmGrid2D_getm_lat'][:]
  lon=nc.variables['getmGrid2D_getm_lon'][:]
  try:
    x0=np.where(lon[0]==ulon)[0][0]
    x1=np.where(lon[-1]==ulon)[0][0]
    y0=np.where(lat[0]==ulat)[0][0]
    y1=np.where(lat[-1]==ulat)[0][0]
  except:
    continue

  for key,value in nc.variables.iteritems():

    if key in set(['getmGrid2D_getm_lat','getmGrid2D_getm_lon','getmGrid3D_getm_layer',
      'getmGrid3D_getm_lat','getmGrid3D_getm_lon','time']):
      continue
    try:
      var=ncout.variables[key]
    except:
      continue

    print f, key, value.shape,  (y1-y0+1, x1-x0+1)

    if (value.shape[-2] == y1-y0+1 and value.shape[-1] == x1-x0+1) :
      if len(value.shape)==4:
         var[:,:,y0:y1+1,x0:x1+1]=value[:,:,:,:]
      elif len(value.shape)==3:
        var[:,y0:y1+1,x0:x1+1]=value[:,:,:]
      elif len(value.shape)==2:
        var[y0:y1+1,x0:x1+1]=value[:,:]
    else:
      print 'skipped ' + key
      break

  nc.close()
ncout.close()
