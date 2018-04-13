#!/usr/bin/env python
#> @brief Stitching script for multiprocessor tiled output of MOSSCO's
#  netcdf components

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2015, 2016, 2017, 2018 Helmholtz Zentrum Geesthacht
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
import os

if len(sys.argv) > 1:
  prefix = sys.argv[1]
else:
  prefix = u"mossco_gfs"
  prefix = u"/Users/lemmen/devel/mossco/setups/sns/netcdf"


if prefix.endswith('.nc'):
  pattern=prefix
  print pattern
  prefix=pattern.split('*')[0]
  if prefix.endswith('_'):
    prefix=prefix[0:-1]
else:
  pattern=prefix + u'.*.nc'


print prefix
print pattern

petlist=[]
key=''
val=''
excl_variables=[]
outfile=prefix + '_stitched.nc'
incl_variables=[]

for i in range(1,len(sys.argv)):
    arg=sys.argv[i]
    if arg[0:2] == '--':
        key=arg.split('=')[0]
        val=arg.split('=')[1]

    if key=='--pet':
        plist=val.split(',')
        for pet in plist:
            pets=pet.split('-')
            if len(pets) == 1:
                p=int(pets[0])
                petlist.append(p)
            else:
                p=range(int(pets[0]),int(pets[1])+1,1)
                petlist.extend(p)

    if key=='--include':
        incl_variables=val.split(',')

    if key=='--out':
        outfile=val

    if key=='--exclude':
        excl_variables=val.split(',')

    if key=='--level':
        levellist=val.split(',')

print petlist
files=glob.glob(pattern)
if len(petlist) > 0 and len(files) > 0:
    fileparts=files[0].split('.')
    files=[]
    print fileparts
    petlen=len(fileparts[-2])
    for i,p in enumerate(petlist):
        if petlen==1: f='%s.%1.1d.nc'%(os.path.join(fileparts[:-2])[0],int(p))
        if petlen==2: f='%s.%2.2d.nc'%(os.path.join(fileparts[:-2])[0],int(p))
        if petlen==3: f='%s.%3.3d.nc'%(os.path.join(fileparts[:-2])[0],int(p))
        if petlen==4: f='%s.%4.4d.nc'%(os.path.join(fileparts[:-2])[0],int(p))
        if petlen==5: f='%s.%5.5d.nc'%(os.path.join(fileparts[:-2])[0],int(p))
        files.append(f)

if len(files)<1:
  print "Did not find any files for pattern "+pattern
  quit()
else:
  print "Using input files " + pattern + " for output file " + outfile


alat={}
alon={}

## Find coord variables

try:
  nc=netcdf.Dataset(files[0],'r')
except:
  print 'Dataset already open'

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

for key,value in alon.iteritems(): alon[key]=np.sort(np.unique(value))
for key,value in alat.iteritems(): alat[key]=np.sort(np.unique(value))

temp={}
for key,value in alon.iteritems():
  temp[key]=value[np.isfinite(value)]
alon=temp

temp={}
for key,value in alat.iteritems():
  temp[key]=value[np.isfinite(value)]
alat=temp

try:
  ncout = netcdf.Dataset(outfile, 'w', format='NETCDF4_CLASSIC')
except:
  ncout.close()
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

if (incl_variables == []):
  incl_variables = nc.variables.keys()

else:
  incl_variables.extend(list(set(alon)))
  incl_variables.extend(list(set(alat)))
  incl_variables.append('time')

  for key,value in nc.variables.iteritems():
    try:
        incl_variables.extend(value.coordinates.split(' '))
    except:
        pass

incl_variables=list(set(incl_variables))
print incl_variables

for key,value in nc.variables.iteritems():
  dims=list(value.dimensions)

  if (key in incl_variables):
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

  try:
    nc=netcdf.Dataset(f,'r')
  except:
    print 'Dataset is already open'

  lat={}
  lon={}
  meta={}

  for item in alat.keys():
    if ncout.variables.has_key(item):
      temp=nc.variables[item][:]
      if type(temp) is np.ndarray: lat[item] = temp[np.isfinite(temp)]
      elif type(temp) is np.ma.core.MaskedArray: lat[item]=temp.compressed()
      else: lat[item] = temp
  for item in alon.keys():
    if ncout.variables.has_key(item):
      temp=nc.variables[item][:]
      if type(temp) is np.ndarray: lon[item] = temp[np.isfinite(temp)]
      elif type(temp) is np.ma.core.MaskedArray: lon[item]=temp.compressed()
      else:
        lon[item] = temp

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
    if not(key in incl_variables): continue

    var=ncout.variables[key]

    if (len(value.shape)) <2:
      #print 'Skipped variable ', key
      continue

    print f, key, value.shape

    dims=list(value.dimensions)
    n=len(dims)
    lbnd=[]
    ubnd=[]
    inlbnd=[]
    inubnd=[]
    for i in range(0,n):
      # set default values for bounds
      lbnd.append(0)
      ubnd.append(len(nc.dimensions[dims[i]]))
      inlbnd.append(0)
      inubnd.append(len(nc.dimensions[dims[i]]))

      # find coordinate variable with axis attribute and same dimension
      for item in coords:
        if ncout.variables[item].dimensions[0]==dims[i]:
          if alon.has_key(item):
            lbnd[i]=meta[item]['x'][0]
            ubnd[i]=meta[item]['x'][1]+1

          else:
            lbnd[i]=meta[item]['y'][0]
            ubnd[i]=meta[item]['y'][1]+1

          try:
            inlbnd[i]= np.min(np.where((nc.variables[item][:]).mask == False))
            inubnd[i]= np.max(np.where((nc.variables[item][:]).mask == False))+1

          except:
            inlbnd[i]=0
            inubnd[i]=len(nc.dimensions[dims[i]])

    if np.any(np.array(inubnd)-np.array(inlbnd) != np.array(ubnd) - np.array(lbnd)) :
      print 'skipped ' + key, lbnd, ubnd, inubnd, inlbnd
      continue


    try:
      if n==1:
        var[lbnd[0]:ubnd[0]]=value[inlbnd[0]:inubnd[0]]
      elif n==2:
        var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1]]=value[inlbnd[0]:inubnd[0],inlbnd[1]:inubnd[1]]
      elif n==3:
        var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1],lbnd[2]:ubnd[2]] \
          =value[inlbnd[0]:inubnd[0],inlbnd[1]:inubnd[1],inlbnd[2]:inubnd[2]]
      else:
        var[lbnd[0]:ubnd[0],lbnd[1]:ubnd[1],lbnd[2]:ubnd[2],lbnd[3]:ubnd[3]] \
        =value[inlbnd[0]:inubnd[0],inlbnd[1]:inubnd[1],inlbnd[2]:inubnd[2],inlbnd[3]:inubnd[3]]
    except:
      print 'skipped ' + key, lbnd, ubnd, value.shape

      continue

    ncout.sync()
    print 'Stitched ' + f + ' ' + key, value.shape

  nc.close()
ncout.close()
