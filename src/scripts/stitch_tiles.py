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
pattern=u"nsbst-physics*.nc"
files=glob.glob(pattern)

if len(files)<1:
  print "Did not find any files for pattern ",pattern

ulon=[]
ulat=[]
ulon2=[]
ulat2=[]

for f in files:
  nc=netcdf.Dataset(f,'r')
  if nc.variables.has_key('getmGrid2D_getm_lat'): ulat2.extend(nc.variables['getmGrid2D_getm_lat'][:])
  if nc.variables.has_key('getmGrid2D_getm_lon'): ulon2.extend(nc.variables['getmGrid2D_getm_lon'][:])
  if nc.variables.has_key('getmGrid3D_getm_lat'): ulat.extend(nc.variables['getmGrid3D_getm_lat'][:])
  if nc.variables.has_key('getmGrid3D_getm_lon'): ulon.extend(nc.variables['getmGrid3D_getm_lon'][:])
  time=nc.variables['time'][:]
  nc.close()

nc=netcdf.Dataset(files[0],'r')
ulon2=set(ulon2)
ulat2=set(ulat2)
ulon=set(ulon)
ulat=set(ulat)

dimDict={'getmGrid2D_getm_1':'lon_2','getmGrid2D_getm_2':'lat_2','getmGrid3D_getm_1':'lon',
         'getmGrid3D_getm_2':'lat','getmGrid3D_getm_3':'height','ungridded00024':'depth','time':'time'}

varDict={'getmGrid2D_getm_lon':'lon_2','getmGrid2D_getm_lat':'lat_2','getmGrid3D_getm_lon':'lon',
         'getmGrid3D_getm_lat':'lat','getmGrid3D_getm_layer':'height'}

ncout = netcdf.Dataset('tile.nc', 'w', format='NETCDF4_CLASSIC')

for key,value in nc.dimensions.iteritems():

  newkey=key    
  if dimDict.has_key(key): newkey = dimDict[key]

  if ncout.dimensions.has_key(newkey): continue

  if newkey=='time' and len(time)>0: ncout.createDimension('time',len(time))
  elif newkey=='lat' and len(ulat)>0: ncout.createDimension('lat',len(ulat))
  elif newkey=='lon' and len(ulon)>0: ncout.createDimension('lon',len(ulon))
  elif newkey=='lat_2' and len(ulat2)>0: ncout.createDimension('lat_2',len(ulat2))
  elif newkey=='lon_2' and len(ulon2)>0: ncout.createDimension('lon_2',len(ulon2))
  else: ncout.createDimension(newkey,len(nc.dimensions[key]))

for key,value in nc.variables.iteritems():
  dims=list(value.dimensions)
  if varDict.has_key(key): key=varDict[key]
  for i in range(0,len(dims)): 
    if dimDict.has_key(dims[i]):
      dims[i]=dimDict[dims[i]]
  try:
    var=ncout.createVariable(key,value.dtype,tuple(dims))
    for att in value.ncattrs():
      if (att != 'coordinates'):
        var.setncattr(att,value.getncattr(att))
  except:
    print 'skipped ' + key

ulon=np.sort(list(ulon))
ulat=np.sort(list(ulat))
ulon2=np.sort(list(ulon2))
ulat2=np.sort(list(ulat2))

if ncout.variables.has_key('lon'): ncout.variables['lon'][:]=ulon
if ncout.variables.has_key('lat'): ncout.variables['lat'][:]=ulat
if ncout.variables.has_key('lon_2'): ncout.variables['lon_2'][:]=ulon2
if ncout.variables.has_key('lat_2'): ncout.variables['lat_2'][:]=ulat2
if ncout.variables.has_key('time'): ncout.variables['time'][:]=time
#ncout.variables['height'][:]=nc.variables['getmGrid3D_getm_layer'][:]
nc.close()

for f in files[:]:
  has2d=True
  has3d=True
  nc=netcdf.Dataset(f,'r')
  if nc.variables.has_key('getmGrid2D_getm_lat'):lat2=nc.variables['getmGrid2D_getm_lat'][:]
  if nc.variables.has_key('getmGrid2D_getm_lon'):lon2=nc.variables['getmGrid2D_getm_lon'][:]
  if nc.variables.has_key('getmGrid3D_getm_lat'):lat=nc.variables['getmGrid3D_getm_lat'][:]
  if nc.variables.has_key('getmGrid3D_getm_lon'):lon=nc.variables['getmGrid3D_getm_lon'][:]
  try:
    x0=np.where(lon[0]==ulon)[0][0]
    x1=np.where(lon[-1]==ulon)[0][0]
    y0=np.where(lat[0]==ulat)[0][0]
    y1=np.where(lat[-1]==ulat)[0][0]
  except:
    print '3D coordinates not found in ', f
    has3d=False
 
  try:
    x20=np.where(lon2[0]==ulon2)[0][0]
    x21=np.where(lon2[-1]==ulon2)[0][0]
    y20=np.where(lat2[0]==ulat2)[0][0]
    y21=np.where(lat2[-1]==ulat2)[0][0]
  except:
   print '2D coordinates not found in ', f
   has2d=False
 
  for key,value in nc.variables.iteritems():

    if key in set(['getmGrid2D_getm_lat','getmGrid2D_getm_lon','getmGrid3D_getm_layer',
      'getmGrid3D_getm_lat','getmGrid3D_getm_lon','time']):
      continue
    try:
      var=ncout.variables[key]
    except:
      continue

    if len(value.shape)<2: continue

    if has2d:
      print f, key, value.shape,  (y21-y20+1, x21-x20+1)
    if has3d:
      print f, key, value.shape,  (y1-y0+1, x1-x0+1)

    if (has3d and value.shape[-2] == y1-y0+1 and value.shape[-1] == x1-x0+1) :
      if len(value.shape)==4:
         var[:,:,y0:y1+1,x0:x1+1]=value[:,:,:,:]
      elif len(value.shape)==3:
        var[:,y0:y1+1,x0:x1+1]=value[:,:,:]
      elif len(value.shape)==2:
        var[y0:y1+1,x0:x1+1]=value[:,:]
    elif (has2d and value.shape[-2] == y21-y20+1 and value.shape[-1] == x21-x20+1) :
      if len(value.shape)==4:
         var[:,:,y20:y21+1,x20:x21+1]=value[:,:,:,:]
      elif len(value.shape)==3:
        var[:,y20:y21+1,x20:x21+1]=value[:,:,:]
      elif len(value.shape)==2:
        var[y20:y21+1,x20:x21+1]=value[:,:]
    else:
      print 'skipped ' + key
      break

  nc.close()
ncout.close()
