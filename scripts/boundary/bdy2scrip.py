#! /usr/bin/env python
# -*- coding: utf-8 -*-
# This python script is part of MOSSCO, it produces from GETM 2D boundary 
# information an ESMF location stream
#
# @copyright (C) 2017, 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

import netCDF4
import re
import numpy as np
import os, sys
import time

def readbdyinfo(info):
        
    if not os.path.exists(info):
        return
        
    # Parse info file, go through WNES boundaries
    bounds=[]
    with open(info,'r') as f:
        ibound=-1
        for line in f:
            if len(line) < 1 or line.startswith('#') or line.startswith('!'): continue

            line = line.split(' ')
            if len(line) == 1:
                ibound=ibound+1
            else:
                bounds.append([ibound,int(line[0]),int(line[1]),int(line[2])])
             
    return bounds


def readtopo(topo):

    if not os.path.exists(topo):
        return

    nc = netCDF4.Dataset(topo)
    ncv = nc.variables


    lonx = np.squeeze(ncv['lonx'][:])  
    latx = np.squeeze(ncv['latx'][:])  

    lonx = np.transpose(lonx)
    latx = np.transpose(latx)

    nc.close()

    return lonx,latx
    
def convertbdy2locstream(info,topo,output):

    boundaryIndices = np.array(readbdyinfo(info))
    lonx, latx = readtopo(topo)

    lon = 0.5*(lonx[1:,1:]+lonx[:-1,:-1])
    lat = 0.5*(latx[1:,1:]+latx[:-1,:-1])
  
    nbdyp = np.sum(boundaryIndices[:,3]-boundaryIndices[:,2])+boundaryIndices.shape[0]
    blon = np.zeros(nbdyp)
    blat = np.zeros(nbdyp)
    blonc = np.zeros((nbdyp,4))
    blatc = np.zeros((nbdyp,4))
    offset = 0

    #print lon.shape,lat.shape
    
    for i in range(0,boundaryIndices.shape[0]):
        btype = boundaryIndices[i,0]
        bnum  = boundaryIndices[i,3]-boundaryIndices[i,2] + 1
        if (btype % 2 == 0):  # western or eastern boundary
            blon[offset:offset + bnum] = lon[boundaryIndices[i,1]-1,boundaryIndices[i,2]-1:boundaryIndices[i,3]]
            blat[offset:offset + bnum] = lat[boundaryIndices[i,1]-1,boundaryIndices[i,2]-1:boundaryIndices[i,3]]
            blonc[offset:offset + bnum,0] = lonx[boundaryIndices[i,1]-1,boundaryIndices[i,2]-1:boundaryIndices[i,3]]
            blatc[offset:offset + bnum,0] = latx[boundaryIndices[i,1]-1,boundaryIndices[i,2]-1:boundaryIndices[i,3]]
            blonc[offset:offset + bnum,1] = lonx[boundaryIndices[i,1],boundaryIndices[i,2]-1:boundaryIndices[i,3]]
            blatc[offset:offset + bnum,1] = latx[boundaryIndices[i,1],boundaryIndices[i,2]-1:boundaryIndices[i,3]]
            blonc[offset:offset + bnum,2] = lonx[boundaryIndices[i,1],boundaryIndices[i,2]:boundaryIndices[i,3]+1]
            blatc[offset:offset + bnum,2] = latx[boundaryIndices[i,1],boundaryIndices[i,2]:boundaryIndices[i,3]+1]
            blonc[offset:offset + bnum,3] = lonx[boundaryIndices[i,1]-1,boundaryIndices[i,2]:boundaryIndices[i,3]+1]
            blatc[offset:offset + bnum,3] = latx[boundaryIndices[i,1]-1,boundaryIndices[i,2]:boundaryIndices[i,3]+1]
        else:  # northern and southern boundary
            blon[offset:offset + bnum] = lon[boundaryIndices[i,2]-1:boundaryIndices[i,3],boundaryIndices[i,1]-1]
            blat[offset:offset + bnum] = lat[boundaryIndices[i,2]-1:boundaryIndices[i,3],boundaryIndices[i,1]-1]
            blonc[offset:offset + bnum,0] = lonx[boundaryIndices[i,2]-1:boundaryIndices[i,3],boundaryIndices[i,1]-1]
            blatc[offset:offset + bnum,0] = latx[boundaryIndices[i,2]-1:boundaryIndices[i,3],boundaryIndices[i,1]-1]
            blonc[offset:offset + bnum,1] = lonx[boundaryIndices[i,2]:boundaryIndices[i,3]+1,boundaryIndices[i,1]-1]
            blatc[offset:offset + bnum,1] = latx[boundaryIndices[i,2]:boundaryIndices[i,3]+1,boundaryIndices[i,1]-1]
            blonc[offset:offset + bnum,2] = lonx[boundaryIndices[i,2]:boundaryIndices[i,3]+1,boundaryIndices[i,1]]
            blatc[offset:offset + bnum,2] = latx[boundaryIndices[i,2]:boundaryIndices[i,3]+1,boundaryIndices[i,1]]
            blonc[offset:offset + bnum,2] = lonx[boundaryIndices[i,2]-1:boundaryIndices[i,3],boundaryIndices[i,1]]
            blatc[offset:offset + bnum,2] = latx[boundaryIndices[i,2]-1:boundaryIndices[i,3],boundaryIndices[i,1]]

        offset = offset + bnum

    writebdyscrip(output,blon,blat,blonc,blatc)
    #locstream = ESMF.LocStream(nbdyp, name="GETM 2D boundary")
    #locstream["ESMF:Lat"] = blat
    #locstream["ESMF:Lon"] = blon
    #locstream["ESMF:Mask"] = np.array([0, 0, 1, 1, 1])

def writebdyscrip(filename,lon,lat,lonc,latc):

    nc=netCDF4.Dataset(filename,'w',format='NETCDF3_CLASSIC')

    nc.createDimension('grid_size',len(lon))
    nc.createDimension('grid_corners',4)
    nc.createDimension('grid_rank',1)

    grid_dims = nc.createVariable('grid_dims','i4',('grid_rank'))
    grid_dims.units = 'unitless'

    grid_imask = nc.createVariable('grid_imask','i4',('grid_size'))
    grid_imask.units = 'unitless'

    grid_center_lat = nc.createVariable('grid_center_lat','f8',('grid_size'))
    grid_center_lat.units='degrees'

    grid_center_lon = nc.createVariable('grid_center_lon','f8',('grid_size'))
    grid_center_lon.units='degrees'

    grid_corner_lat = nc.createVariable('grid_corner_lat','f8',('grid_size','grid_corners'))
    grid_corner_lat.units='degrees'

    grid_corner_lon = nc.createVariable('grid_corner_lon','f8',('grid_size','grid_corners'))
    grid_corner_lon.units='degrees'

# Meta data
    nc.history = 'Created ' + time.ctime(time.time()) + ' by ' + sys.argv[0]
    nc.creator = 'Carsten Lemmen <carsten.lemmen@hzg.de>'
    nc.license = 'Creative Commons Attribution Share-alike (CC-BY-SA)'
    nc.copyright = 'Helmholtz-Zentrum Geesthacht'
    nc.Conventions = 'SCRIP'

# Values
    grid_dims[:]=lon.shape
    grid_imask[:]=np.zeros(lon.shape,dtype=int)

    grid_center_lat[:]=lat
    grid_center_lon[:]=lon

# Corners from ll anticlockwise to ul
    grid_corner_lat[:,:]=latc
    grid_corner_lon[:,:]=lonc

    nc.close()


if __name__ == '__main__':

  basedir=os.environ['MOSSCO_SETUPDIR']
  bdyinfo = os.path.join(basedir, 'sns', 'bdyinfo.dat')
  topo = os.path.join(basedir, 'sns', 'Topo', 'topo.nc')

  locdata = re.sub('.dat','_locstream.nc',bdyinfo)
  
  if os.path.exists(topo) and os.path.exists(bdyinfo): 
      convertbdy2locstream(bdyinfo,topo,locdata)



