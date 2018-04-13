# -*- coding: utf-8 -*-
"""
# This script is is part of MOSSCO. It creates a 1D (point/node data)
# UGRID/CF compliant NetCDF file (currently for Weser and Elbe rivers)
#
# @copyright (C) 2018 Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister <richard.hofmeister@hzg.de>
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
"""
import os,sys
from scipy.interpolate import LinearNDInterpolator as li
import netCDF4
import numpy as np
from datetime import datetime,timedelta
import netcdftime
import pickle

if len(sys.argv)<2:
  print('usage: trim2getmbdy.py infile')
  infile = 'trim_1.e.coastdatII_2016.nc'
  print('  use %s'%infile)
else:
  infile=sys.argv[1]

torigin=datetime(2000,1,1,0,0,0)
unitstr='seconds since 2000-01-01 00:00:00'
utime=netcdftime.utime(unitstr)

inc=netCDF4.Dataset(infile)
incv=inc.variables
iutime=netcdftime.utime(incv['time'].units)
tlen=len(inc.dimensions['time'])
date_str=str(iutime.origin)[:10]
year_str=infile[-7:-3]


bdylon,bdylat = np.load('bdylonlat.pickle')
bdylon=list(bdylon)
bdylat=list(bdylat)

nbdyp=len(bdylon)
bdypoints = list(zip(bdylon,bdylat))

# restrict data area to sns
xslice=slice(52,111)
yslice=slice(69,116)

if True:
   # get or initialise Triangulation
   gnc=inc
   gncv=gnc.variables
   glon=gncv['lon'][yslice,xslice]
   glat=gncv['lat'][yslice,xslice]   
   gpoints = list(zip(glon.flat,glat.flat))

bdyfilename = 'bdy_2d_%s.nc' %(year_str,)
if os.path.exists(bdyfilename):
    onc=netCDF4.Dataset('bdy_2d_%s.nc' %(year_str,),'a',format='NETCDF3_CLASSIC')
else:
    onc=netCDF4.Dataset('bdy_2d_%s.nc' %(year_str,),'w',format='NETCDF3_CLASSIC')

onc.Conventions = 'CF-1.7, UGRID-1.0'
onc.license = 'GPL 3.0 or later'
onc.contact='Carsten Lemmen, <carsten.lemmen@hzg.de>'
onc.copyright= 'Copyright ' + str(datetime.now())[0:4] + ' Helmholtz-Zentrum Geesthacht'
onc.script=sys.argv[0]
onc.user=os.environ['USER']
onc.working_directory=os.environ['PWD']
onc.description='GETM 2D boundary file with elevation/velocity from TRIM'

if 'boundary_locstream' not in onc.variables: 
    m = onc.createVariable('boundary_locstream','i4',())
    m.cf_role='mesh_topology'
    m.topology_dimension = 1.
    m.node_coordinates='lon lat'

if 'time' not in onc.dimensions: onc.createDimension('time',None)
if 'nbdyp' not in onc.dimensions: onc.createDimension('nbdyp',nbdyp)

if 'time' not in onc.variables:
    tv=onc.createVariable('time','f8',('time',))
    tv.units=unitstr
    tv[0:tlen]=utime.date2num(iutime.num2date(incv['time'][0:tlen]))
else:
    tv=onc.variables['time']

if 'lat' not in onc.variables:
    lat=onc.createVariable('lat','f8',('nbdyp'))
    lat.units = gncv['lat'].units
    lat.standard_name = 'latitude'
else:
    lat=onc.variables['lat']

if 'lon' not in onc.variables:
    lon=onc.createVariable('lon','f8',('nbdyp'))
    lon.units = gncv['lon'].units
    lon.standard_name = 'longitude'
else:
    lon=onc.variables['lon']

if 'u' in incv:
    if 'u' not in onc.variables:
        uv=onc.createVariable('u','f4',('time','nbdyp'),fill_value=-1E30)
        uv.units='m s-1'
        uv.location='node'
        uv.mesh='boundary_locstream'
    else:
        uv=onc.variables['u']

if 'v' in incv:
    if 'v' not in onc.variables:
        vv=onc.createVariable('v','f4',('time','nbdyp'),fill_value=-1E30)
        vv.units='m s-1'
        vv.location='node'
        vv.mesh='boundary_locstream'
    else:
        vv=onc.variables['v']
        

if 'e' in incv:
    if 'elev' not in onc.variables:
        e = incv['e']
        ev=onc.createVariable('elev','f4',('time','nbdyp'),fill_value=-1E30)
        ev.units='m'
        ev.standard_name = 'sea_surface_elevation'
        ev.location='node'
        ev.mesh='boundary_locstream'
    else:
       ev=onc.variables['elev']

onc.sync()

if 'e' in incv:
    elon=gncv['lon'][yslice,xslice]
    elat=gncv['lat'][yslice,xslice]   
    epoints = list(zip(elon.flat,elat.flat))
    lo=li(epoints,list(incv['lon'][yslice,xslice].flat))
    lon[:]=lo(bdypoints)
    la=li(epoints,list(incv['lat'][yslice,xslice].flat))  
    lat[:]=la(bdypoints)
    print 'Adding elevation, lat, lon to ' + bdyfilename

if 'u' in incv:
    ulon=gncv['lon_u'][yslice,xslice]
    ulat=gncv['lat_u'][yslice,xslice]   
    upoints = list(zip(ulon.flat,ulat.flat))
    print 'Adding u component to ' + bdyfilename

if 'v' in incv:
    vlon=gncv['lon_v'][yslice,xslice]
    vlat=gncv['lat_v'][yslice,xslice]   
    vpoints = list(zip(vlon.flat,vlat.flat))
    print 'Adding v component to ' + bdyfilename

onc.sync()


for t in range(0,tlen,1): #range(tlen):
    if t%1000==0:  
        print(' step %d'%t)
        onc.sync()
    if 'e' in incv:
        eintp = li(epoints,list(incv['e'][t,yslice,xslice].flat))
        ev[t,:]=eintp(bdypoints)
    if 'u' in incv:
        uintp = li(upoints,list(incv['u'][t,0,yslice,xslice].flat))
        uv[t,:]=uintp(bdypoints)
    if 'v' in incv:
        vintp = li(vpoints,list(incv['v'][t,0,yslice,xslice].flat))
        vv[t,:]=vintp(bdypoints)
 
onc.close()
inc.close()
