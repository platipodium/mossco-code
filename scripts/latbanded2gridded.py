# -*- coding: utf-8 -*-
"""
# This script is is part of MOSSCO. It creates from a netcdf file in
# latitude-banded grid-mapping "1D binned sinusoidal"
#
# @copyright (C) 2016 Helmholtz-Zentrum Geesthacht
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
import numpy as np
import time
import re
import os

if __name__ == '__main__':

    # Handle command line arguments, 
    # first argument is the input file name
    # second argument is the comma-separated corner 
    # specification in (ll_lon,ll_lat,ur_lon,ur_lat)
    if len(sys.argv)>1:
        basename=sys.argv[1]
    else:
        basename = 'ESACCI-OC-L3S-CHLOR_A-MERGED-1D_DAILY_4km_SIN_PML_OC4v6-20030218-fv2.0.nc'

    if len(sys.argv)>2:
        cornerstring=sys.argv[2]
    else:
        #cornerstring='-180,-90,180,90'
        cornerstring='0,48,10,58'
        #cornerstring='6,53,8,55'

    # Test for correctness of input arguments
    if not os.path.exists(basename):
        print 'File ' + basename + ' does not exist.'  

    corners=np.asarray(cornerstring.split(','), dtype=float)
    if len(corners)!= 4:
        print 'Invalid corner specification ' + cornerstring
        

    nc = netCDF4.Dataset(basename,'r')
    ncv = nc.variables
    lon = np.squeeze(ncv['lon'][:])
    lat = np.squeeze(ncv['lat'][:])
    
    dimName = ncv['lon'].dimensions[0]

    inCorner = np.where(lon > corners[0])[0]
    inCorner = inCorner[np.where(lon[inCorner] < corners[2])]
    inCorner = inCorner[np.where(lat[inCorner] > corners[1])]
    inCorner = inCorner[np.where(lat[inCorner] < corners[3])]

    dlat = corners[3] - corners[1]
    dlon = corners[2] - corners[0]

    lon = lon[inCorner]
    lat = lat[inCorner]
  
    n=len(lon)
    ny=np.int(np.unique(lat).size)
    latc = np.unique(lat)

    lonsize=np.zeros((ny,))
    for i in range(0, ny):
        inLat = np.where(np.abs(lat - latc[i]) < dlat/2.0/ny)[0]    
        lonsize[i]=inLat.size

    nx=np.max(lonsize)
    dx=np.ceil(nx/dlon)
    nx=np.int(dlon*dx)
  
    latc = np.unique(lat)
    lonc = corners[0] + np.linspace(dlon/nx/2,dlon-dlon/nx/2,nx) 
    
    varm={}
    for key,value in ncv.iteritems():
        if not dimName in value.dimensions: continue
        #if not len(key) == 3: continue
        #if not u'coordinates' in value.ncattrs(): continue
        #if not (value.coordinates == u'lat lon'): continue
            
        # Don't include these masked fields (yet)
        #if key.endswith('nobs'): continue
            
        # actually only consider chl_a here
        if key !=  'chlor_a': continue       
        varm[key]=np.zeros((ny,nx)) - 999.999
        if '_FillValue' in value.ncattrs():
            varm[key][:,:]=value._FillValue
            
    
    for i in range(0, ny):
        inLat = np.where(np.abs(lat - latc[i]) < dlat/2.0/ny)[0]  
        if inLat.size < 1: continue
        inLon=np.floor((lon[inLat]-corners[0])/dlon*nx)
        
        inLon[np.where(inLon < 0)] = 0
        inLon[np.where(inLon > nx-1)] = nx-1
        #print inLat.size
        
        inLon = np.asarray(inLon,dtype=int)
        inLat = np.asarray(inLat,dtype=int)
        
        for key,value in varm.iteritems():
            if type(ncv[key][0:1]) is np.ndarray:
                varval=ncv[key][inCorner]                
            else:
                varval=(ncv[key][:].data)[0][inCorner]
                
            value[i,inLon] = varval[inLat]
           
        if i % 10 == 0: print 'got lat rows up to ', i , ' of ', ny
    
    ncout=netCDF4.Dataset(re.sub('.nc','',basename) + '_gridded.nc', 'w', format='NETCDF3_CLASSIC')
    ncout.createDimension('lon',nx)
    ncout.createDimension('lat',ny)
    #ncout.createDimension('time',ntime)

    lonv = ncout.createVariable('lon','f4',('lon'))
    lonv.units='degree_east'
    lonv.long_name='longitude'
    lonv.standard_name='longitude'
    lonv.axis = 'X'

    latv = ncout.createVariable('lat','f4',('lat'))
    latv.units='degree_north'
    latv.long_name='latitude'
    latv.standard_name='latitude'
    latv.axis = 'Y'

    # Meta data
    ncout.history = 'Created ' + time.ctime(time.time()) + ' by ' + sys.argv[0]
    ncout.creator = 'Carsten Lemmen <carsten.lemmen@hzg.de>'
    ncout.license = 'Creative Commons share-alike (CCSA)'
    ncout.copyright = 'Helmholtz-Zentrum Geesthacht'
    ncout.Conventions = 'CF-1.6'

    lonv[:] = lonc
    latv[:] = latc

    # Now the variables
    for key,value in varm.iteritems():

        varName = key
        if (key == 'lat'): varName = 'clat'      
        if (key == 'lon'): varName = 'clon'      
        
        if '_FillValue' in ncv[key].ncattrs():
            varv = ncout.createVariable(varName,'f4',('lat','lon'), fill_value=ncv[key]._FillValue)
        else:
            varv = ncout.createVariable(varName,'f4',('lat','lon'), fill_value=-999.999)

        for att in ncv[key].ncattrs():
            if (att == 'units'):
                ncout.units= ncv[key].units               
            if (att == 'coordinates'):
                ncout.coordinates= ncv[key].coordinates               
            if (att == 'standard_name'):
                ncout.standard_name= ncv[key].standard_name               
        varv[:] = value

    nc.close()
    ncout.close()    
    
    
    
    
    
 