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
import sys

if len(sys.argv) > 1:
  prefix = sys.argv[1]
else:
  prefix = u"netcdf_out"
  prefix = u"/Volumes/Kiwi/output/sn-m/mossco_gffrr"

if len(sys.argv) > 2:
  excl_variables = sys.argv[2].split(',')
else:
  excl_variables = []

pattern=prefix + u'.*.nc'
files=glob.glob(pattern)
    
outfile=prefix + '_stitched.nc'

if len(files)<1:
  print "Did not find any files for pattern "+pattern
  
alat={}
alon={}

## Find coord variables  
nc=netcdf.Dataset(files[0],'r')
for key, value in nc.variables.iteritems():
  dim=value.dimensions
  #print key, dim
  if len(value.dimensions) != 1 : continue
  if key.endswith('_lat'): alat[key]=[]
  elif key.endswith('_lon'): alon[key]=[]
  elif key.endswith('_X'): alon[key]=[]
  elif key.endswith('_Y'): alat[key]=[]
  elif key.endswith('_x'): alon[key]=[]
  elif key.endswith('_y'): alat[key]=[]
nc.close()

for item in alat.keys():
  print "Found latitude/Y information " + item
for item in alon.keys():
  print "Found longitude/X information " + item

if len(alon)<1:
  print "Found no longitude/X information"
if len(alat)<1:
  print "Found no latitude/Y information"

for f in files:
  nc=netcdf.Dataset(f,'r')
  
  for item in alat.keys():
    if nc.variables.has_key(item) and alat.has_key(item): 
      alat[item].extend(nc.variables[item][:])
  for item in alon.keys(): 
    if nc.variables.has_key(item) and alon.has_key(item):
      alon[item].extend(nc.variables[item][:])
  nc.close()

nc=netcdf.Dataset(files[0],'r')
time=nc.variables['time'][:]

for key,value in alon.iteritems(): alon[key]=np.sort(list(set(value)))
for key,value in alat.iteritems(): alat[key]=np.sort(list(set(value)))

temp={}
for key,value in alon.iteritems():
  if np.all(np.isfinite(value)):
    temp[key]=value
    
alon=temp

temp={}
for key,value in alat.iteritems():
  if np.all(np.isfinite(value)):
    temp[key]=value
alat=temp

ncout = netcdf.Dataset(outfile, 'w', format='NETCDF4_CLASSIC')


for key,value in alon.iteritems():
  dim=nc.variables[key].dimensions[0]
  if ncout.dimensions.has_key(dim): continue
  ncout.createDimension(dim,len(alon[key]))
for key,value in alat.iteritems():
  dim=nc.variables[key].dimensions[0]
  if ncout.dimensions.has_key(dim): continue
  ncout.createDimension(dim,len(alat[key]))

for key,value in nc.dimensions.iteritems():
  if ncout.dimensions.has_key(key): continue

  if key=='time' and len(time)>0: ncout.createDimension('time',len(time))
  else: ncout.createDimension(key,len(nc.dimensions[key]))

# Create all variables that are in nc also in ncout.  Be careful with _FillValue
# attribute, as adding this after variable creation causes spurious "variable not
# found" errors.

if (excl_variables == []):
  excl_variables = nc.variables.keys()
else:
  excl_variables.extend(list(set(alon)))
  excl_variables.extend(list(set(alat)))
  excl_variables.append('time')

for key,value in nc.variables.iteritems():
  dims=list(value.dimensions)

  if (key in excl_variables):
    try:
      var=ncout.createVariable(key,value.dtype,tuple(dims),fill_value=value.getncattr('_FillValue'))
    except:
      var=ncout.createVariable(key,value.dtype,tuple(dims))

    for att in value.ncattrs():
      if att == '_FillValue': continue
      var.setncattr(att,value.getncattr(att))
    print 'Created for output variable ', key , tuple(dims)

nc.close()

# Now add values to time and coordinate variables, close
if ncout.variables.has_key('time'): ncout.variables['time'][:]=time

for item in alat.keys():
  if ncout.variables.has_key(item): 
    print item, len(ncout.variables[item][:]), len(alat[item])
    ncout.variables[item][:]=alat[item]
  else:
    print 'Could not find item ' , item, ' in ncout'
for item in alon.keys():
  if ncout.variables.has_key(item): ncout.variables[item][:]=alon[item]
  else:
    print 'Could not find item ' , item, ' in ncout'

for f in files[:]:
  nc=netcdf.Dataset(f,'r')
  lat={}
  lon={}
  meta={}

  for item in alat.keys():
    if nc.variables.has_key(item): lat[item]=nc.variables[item][:]
  for item in alon.keys():
    if ncout.variables.has_key(item): lon[item]=nc.variables[item][:]

  for item in lat.keys():
    if alat.has_key(item):
      try:
        if not meta.has_key(item): meta[item]={}
        meta[item]['y']= (np.where(lat[item][0]==alat[item])[0][0], np.where(lat[item][-1]==alat[item])[0][0])
      except:
        print "Coordinate ", item, ' found not match in ', f
  for item in lon.keys():
    if alon.has_key(item):
      try:
        if not meta.has_key(item): meta[item]={}
        meta[item]['x']= (np.where(lon[item][0]==alon[item])[0][0], np.where(lon[item][-1]==alon[item])[0][0])
      except:
        print 'Coordinate ', item, ' found no match in ', f

  coords=alon.keys()
  coords.extend(alat.keys())

  for key,value in nc.variables.iteritems():

    if key in ['time']: continue
    if key in coords: continue
    if not(key in excl_variables): continue

    var=ncout.variables[key]

    if (len(value.shape)) <2:
      #print 'Skipped variable ', key
      continue

    print f, key, value.shape

    dims=list(value.dimensions)
    n=len(dims)
    lbnd=[]
    ubnd=[]
    for i in range(0,n):
      lbnd.append(0)
      ubnd.append(len(ncout.dimensions[dims[i]]))
      for item in coords:
        if ncout.variables[item].dimensions[0]==dims[i]:
          if alon.has_key(item):
            lbnd[i]=meta[item]['x'][0]
            ubnd[i]=meta[item]['x'][1]+1
          else:
            lbnd[i]=meta[item]['y'][0]
            ubnd[i]=meta[item]['y'][1]+1
      #if lnbd[i]==0: lbnd

    #print lbnd, ubnd, value.shape

    success=True
    
    if n==1 and (var[lbnd[0]:ubnd[0]]).shape == value.shape:  
      var[lbnd[0]:ubnd[0]]=value[:]
    elif n==2 and  (var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1]]).shape == value.shape:
      var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1]]=value[:,:]
    elif n==3 and (var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1],lbnd[2]:ubnd[2]]).shape == value.shape:
      var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1],lbnd[2]:ubnd[2]]=value[:,:,:]
    elif n==4 and (var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1],lbnd[2]:ubnd[2],lbnd[3]:ubnd[3]]).shape == value.shape:
      var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1],lbnd[2]:ubnd[2],lbnd[3]:ubnd[3]]=value[:,:,:,:]
    else:
      success=False

    if not success:
      print 'skipped ' + key
      continue

    ncout.sync()
    print 'Stitched ' + f + ' ' + key, value.shape

  nc.close()
ncout.close()
