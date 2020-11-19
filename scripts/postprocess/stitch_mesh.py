#!/usr/bin/env python
#> @brief Stitching script for multiprocessor mesh output of MOSSCO's
#  netcdf components

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2018--2020 Helmholtz Zentrum Geesthacht
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

prefix = u"schism_mesh_output"
petlist=[]
timelist=[]
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
    else:
        prefix=arg
        if prefix.endswith('.nc'):
            pattern=prefix
            prefix=pattern.split('*')[0]
            if prefix.endswith('_'):
                prefix=prefix[0:-1]
            else:
                pattern=prefix + u'.*.nc'
        continue

    if key=='--include':
        incl_variables=val.split(',')

    if key=='--out':
        outfile=val

    if key=='--exclude':
        excl_variables=val.split(',')

    if key=='--level':
        levellist=[np.int(x) for x in val.split(',')]

    if key=='--pet':
        petlist=[np.int(x) for x in val.split(',')]

    if key=='--time':
        timelist=[np.int(x) for x in val.split(',')]


#print petlist
pattern=prefix + u'.*.nc'
files=glob.glob(pattern)
if len(petlist) > 0 and len(files) > 0:
    fileparts=files[0].split('.')
    files=[]
    print (fileparts)
    petlen=len(fileparts[-2])
    for i,p in enumerate(petlist):
        if petlen==1: f='%s.%1.1d.nc'%('.'.join(fileparts[:-2]),int(p))
        if petlen==2: f='%s.%2.2d.nc'%('.'.join(fileparts[:-2]),int(p))
        if petlen==3: f='%s.%3.3d.nc'%('.'.join(fileparts[:-2]),int(p))
        if petlen==4: f='%s.%4.4d.nc'%('.'.join(fileparts[:-2]),int(p))
        if petlen==5: f='%s.%5.5d.nc'%('.'.join(fileparts[:-2]),int(p))
        files.append(f)

if len(files)<1:
  print ("Did not find any files for pattern "+pattern)
  quit()
else:
  print("Using input files " + pattern + " for output file " + outfile)

# Assembling a set of global node and element ids

node_id_max = 0
elem_id_max = 0
for f in files:
  nc=netcdf.Dataset(f,'r')

  if nc.variables['mesh_global_node_id'].dimensions[0] == 'time':
      node_id_max = np.max([np.max(nc.variables['mesh_global_node_id'][0,:]),node_id_max])
      elem_id_max = np.max([np.max(nc.variables['mesh_global_element_id'][0,:]),elem_id_max])
  else:
      node_id_max = np.max([np.max(nc.variables['mesh_global_node_id'][:]),node_id_max])
      elem_id_max = np.max([np.max(nc.variables['mesh_global_element_id'][:]),elem_id_max])
  nc.close()

elem_id  = np.arange(elem_id_max,dtype='int32')
elem_lon = np.zeros(elem_id.shape,dtype='float')
elem_lat = np.zeros(elem_id.shape,dtype='float')

node_id  = np.arange(node_id_max,dtype='int32')
node_lon = node_lon = np.zeros(node_id.shape,dtype='float')
node_lat = node_lat = np.zeros(node_id.shape,dtype='float')

elem_node_conn = np.zeros((len(elem_id),3),dtype='int32')

for f in files:
  nc=netcdf.Dataset(f,'r')

  # Note that in the .nc file, the node id's are 1-based
  if nc.variables['mesh_global_node_id'].dimensions[0] == 'time':
      node_id_global=list(nc.variables['mesh_global_node_id'][0,:]-1)
      elem_id_global=list(nc.variables['mesh_global_element_id'][0,:]-1)
      elem_node_conn[elem_id_global,:]=nc.variables['mesh_element_node_connectivity'][0,0:3,:].T
  else:
      node_id_global=list(nc.variables['mesh_global_node_id'][:]-1)
      elem_id_global=list(nc.variables['mesh_global_element_id'][:]-1)
      elem_node_conn[elem_id_global,:]=nc.variables['mesh_element_node_connectivity'][0:3,:].T

  node_lon[node_id_global]=nc.variables['mesh_node_lon'][:]
  node_lat[node_id_global]=nc.variables['mesh_node_lat'][:]

  elem_lon[elem_id_global]=nc.variables['mesh_element_lon'][:]
  elem_lat[elem_id_global]=nc.variables['mesh_element_lat'][:]

  nc.close()

nc=netcdf.Dataset(files[0],'r')
time=nc.variables['time'][:]

try:
  ncout = netcdf.Dataset(outfile, 'w', format='NETCDF4_CLASSIC')
except:
  ncout.close()
  ncout = netcdf.Dataset(outfile, 'w', format='NETCDF4_CLASSIC')

for key,value in nc.dimensions.items():

    if key in ncout.dimensions: continue

    if key == 'time':
        if (len(timelist)>0):
            ncout.createDimension('time',len(timelist))
        elif len(time)>0:
            ncout.createDimension('time',len(time))
    elif key == 'mesh_element':
        ncout.createDimension(key,elem_id_max)
    elif key == 'mesh_node':
        ncout.createDimension(key,node_id_max)
    elif key == 'ungridded00004':
        ncout.createDimension(key,3)
    else:
        ncout.createDimension(key,len(nc.dimensions[key]))

    print ('Created dimension ' + key + ' with length ' + str(len(ncout.dimensions[key])))

# Create all variables that are in nc also in ncout.  Be careful with _FillValue
# attribute, as adding this after variable creation causes spurious "variable not
# found" errors.

if (incl_variables == []):
  incl_variables = nc.variables.keys()

else:
  incl_variables.extend(['mesh_node_lat','mesh_node_lon','mesh_element_node_connectivity','mesh_element_lon','mesh_element_lat'])
  incl_variables.append('time')

  for key,value in nc.variables.items():
    try:
        incl_variables.extend(value.coordinates.split(' '))
    except:
        pass

incl_variables=set(incl_variables)
try: incl_variables.remove('mesh_global_element_id')
except: pass
try: incl_variables.remove('mesh_global_node_id')
except: pass

incl_variables=list(set(incl_variables))
print( incl_variables)

for key,value in nc.variables.items():
  dims=list(value.dimensions)

  if key == 'mesh_element_node_connectivity':
      if 'time' in value.dimensions: dims = list(value.dimensions[1:])
      dims = dims[::-1]

  if (key in incl_variables):
    try:
      var=ncout.createVariable(key,value.dtype,tuple(dims),fill_value=value.getncattr('_FillValue'))
    except:
      var=ncout.createVariable(key,value.dtype,tuple(dims))

    for att in value.ncattrs():
      if att == '_FillValue': continue
      var.setncattr(att,value.getncattr(att))

    if np.any([dim.startswith('mesh_') for dim in dims]):
        var.setncattr('mesh', 'mesh_topology')
    print ('Created for output variable ', key , tuple(dims))

# Now add values to time and coordinate variables
if 'time' in ncout.variables:
    if len(timelist)>0:
        ncout.variables['time'][:]=time[timelist]
    else:
        ncout.variables['time'][:]=time
if 'mesh_node_lat' in ncout.variables: ncout.variables['mesh_node_lat'][:]=node_lat
if 'mesh_node_lon' in ncout.variables: ncout.variables['mesh_node_lon'][:]=node_lon
if 'mesh_element_lat' in ncout.variables: ncout.variables['mesh_element_lat'][:]=elem_lat
if 'mesh_element_lon' in ncout.variables: ncout.variables['mesh_element_lon'][:]=elem_lon

if 'mesh_element_node_connectivity' in ncout.variables:
    ncout.variables['mesh_element_node_connectivity'][:,:]=elem_node_conn[:,0:3]
    ncout.variables['mesh_element_node_connectivity'].standard_name = "face_node_connectivity" ;
    ncout.variables['mesh_element_node_connectivity'].start_index = int(np.min(elem_node_conn[:,0:3]));

    var = ncout.createVariable('mesh_topology','int32',dimensions=())
    var.cf_role = "mesh_topology"
    var.topology_dimension = 2. ;
    var.node_coordinates = "mesh_node_lon mesh_node_lat" ;
    var.face_node_connectivity = "mesh_element_node_connectivity" ;

ncout.Conventions = "CF-1.7, UGRID-1.0" ;

for key, value in nc.variables.items():

    if 'mesh_node'    in value.dimensions: continue
    if 'mesh_element' in value.dimensions: continue

    if not key in ncout.variables.keys(): continue

    if 'time' in value.dimensions and len(timelist)>0:
        ncout.variables[key][:]=nc.variables[key][timelist]
    else:
        ncout.variables[key][:]=nc.variables[key][:]
    print ('Added non-meshed information ' + key)

nc.close()

for f in files[:]:

    try:
        nc=netcdf.Dataset(f,'r')
    except:
        print( 'Dataset is already open')

    # Note that in the .nc file, the node id's are 1-based
    if nc.variables['mesh_global_node_id'].dimensions[0] == 'time':
        node_id_global=list(nc.variables['mesh_global_node_id'][0,:]-1)
        elem_id_global=list(nc.variables['mesh_global_element_id'][0,:]-1)
    else:
        node_id_global=list(nc.variables['mesh_global_node_id'][:]-1)
        elem_id_global=list(nc.variables['mesh_global_element_id'][:]-1)

    for key, value in nc.variables.items():

        if not key in ncout.variables: continue

        for att in value.ncattrs():
            if att == '_FillValue': continue
            ncout.variables[key].setncattr(att, value.getncattr(att))

    ncout.variables['mesh_node_lon'].units='degrees_east'
    ncout.variables['mesh_node_lat'].units='degrees_north'
    ncout.variables['mesh_node_lon'].standard_name='longitude'
    ncout.variables['mesh_node_lat'].standard_name='latitude'

    for key, value in nc.variables.items():

        if key in ['mesh_node_lat','mesh_node_lon','mesh_element_node_connectivity','mesh_element_lon','mesh_element_lat','time']: continue

        if not key in ncout.variables.keys(): continue

        if 'mesh_node' in value.dimensions:
            mesh_index = node_id_global
            mesh_location = u'mesh_node'
        elif 'mesh_element' in value.dimensions:
            mesh_index=elem_id_global
            mesh_location = u'mesh_element'
        else:
            continue


        var = ncout.variables[key]
        mesh_index_position = value.dimensions.index(mesh_location)

        try:
            if 'time' in value.dimensions:
                time_index_position = value.dimensions.index('time')

            if len(value.shape) == 1:
                if 'time' in value.dimensions and len(timelist)>0:
                    var[mesh_index] == value[timelist].copy()
                else:
                    var[mesh_index] == value[:].copy()
            elif len(value.shape) == 2:
                print (key,value.dimensions,mesh_location,len(mesh_index),mesh_index_position)
                if mesh_index_position == 0:
                    var[mesh_index,:] = value[:].copy()
                else:
                    if 'time' in value.dimensions and len(timelist)>0:
                        var[:,mesh_index] = value[timelist,:].copy()
                    else:
                        #print(value.shape, var.shape,len(mesh_index))
                        var[:,mesh_index] = value[:].copy()
            elif len(value.shape) == 3:
                if mesh_index_position == 0:
                    var[mesh_index,:,:] = value[:].copy()
                elif mesh_index_position == 1:
                    if 'time' in value.dimensions and len(timelist)>0:
                        var[:,mesh_index,:] = value[timelist,:,:].copy()
                    else:
                        var[:,mesh_index,:] = value[:].copy()
                else:
                    if 'time' in value.dimensions and len(timelist)>0:
                        var[:,:,mesh_index] = value[timelist,:,:]
                    else:
                        var[:,:,mesh_index] = value[:].copy()
            else:
                print ('Not implemented. skipped ' + key, value.shape)

            ncout.sync()
            print ('Stitched ' + f + ' ' + key, value.shape)
        except:
            print ('Did not stitch ' + f + ' ' + key, value.shape)
    nc.close()

ncout.close()
