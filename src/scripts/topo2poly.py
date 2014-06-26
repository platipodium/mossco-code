# -*- coding: utf-8 -*-
"""
Created on Fri Jun 20 12:28:35 2014

@author: lemmen
"""
from pylab import *
import netCDF4

def read_topo_ncdf(ncfile):
  nc = netCDF4.Dataset(ncfile)
  ncv = nc.variables
  z = squeeze(ncv['bathymetry'][:])  
  lonx = squeeze(ncv['lonx'][:])  
  latx = squeeze(ncv['latx'][:])  

  nc.close()

  return lonx,latx,z

if __name__ == '__main__':

  lonx, latx, z = read_topo_ncdf('/Users/lemmen/devel/data/topo.nc')

#.node files
#First line: <# of vertices> <dimension (must be 2)> <# of attributes> <# of boundary markers (0 or 1)>
#Remaining lines: <vertex #> <x> <y> [attributes] [boundary marker]

  fid=open('/Users/lemmen/devel/GIS/Triangle/topo.node','w')
  fid.write(str(size(z[~z.mask])) + ' 2 1 1\n')  
  
  k=0
  for i in range(0,shape(z)[0]):
     for j in range(0,shape(z)[1]):
         
         if type(z[i][j]) is not numpy.float32: 
             continue
  
         border=0
         if ( i==0 or j==0 or i== shape(z)[0]-1 or j==shape(z)[1]-1) : 
             border=1
         else:
             for ii in [-1, 0, 1]:
                 for jj in [-1, 0, 1]:
                     if type(z[i+ii][j+jj]) is not numpy.float32: 
                         border=1
             
         lat=(latx[i][j]+latx[i+1][j+1])/2.0        
         lon=(lonx[i][j]+lonx[i+1][j+1])/2.0
         #print i, j, lon, lat, z[i][j]
         k=k+1
 
         fid.write(str(k) + ' ' + str(lon) + ' ' + str(lat) + ' ')
         fid.write(str(z[i][j]) + ' ' + str(border) + '\n')

  fid.close()