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

  basename = '/Users/lemmen/devel/data/topo.nc'
  lonx, latx, z = read_topo_ncdf(basename)
  lat=latx[:]
  lon=lonx[:]
  nx=shape(lonx)[0]
  ny=shape(lonx)[1]

  nnode = size(lat)
  max_node_per_element=4
  start_index=1
  nelement=size(z)
  element=z
  element[0:nelement-1]=numpy.array(range(0,nelement),'i8').reshape(nx-1,ny-1)
  
  node=lonx
  node[0:nnode-1]=numpy.array(range(0,nnode),'i8').reshape(nx,ny)
  
  nv=numpy.array((nelement,4),'i8')
  ev=numpy.array([nnode,4],'i8')

  # go through all nodes
  for i in range(0,nx):
    for j in range(0,ny):
      k=i*ny+j
      print i,j,k
      if (i>0 and j<ny-1):
        ev[k][0]=element[i-1][j]
      if (i<nx-1 and j<ny-1):
        ev[k][1]=element[i][j]
      if (i<nx-1 and j>0):
        ev[k][2]=element[i][j-1]
      if (i>0 and j>0):
        ev[k][3]=element[i-1][j-1]


  nc=netCDF4.Dataset(basename + '_ugrid.nc','w',format='NETCDF3_CLASSIC')
 
  nc.createDimension('node',nnode)
  nc.createDimension('element',nelement)
  nc.createDimension('node_per_element',max_node_per_element)

 
 
  m = nc.createVariable('quadrangle_mesh','i4',())
  m.cf_role='mesh_topology'
  m.topology_dimension = 2.
  m.node_coordinates='lon lat'
  m.face_node_connectivity='nv'

  nv = nc.createVariable('nv','i',('element','node_per_element'))
  nv.start_index = start_index
 # nv[:]=elements["nodes"]

  lon = nc.createVariable('lon','f8',('node',))
  lon.units = 'degrees_east'
  #lon[:]=nodes['lon']

  lat = nc.createVariable('lat','f8',('node',))
  lat.units = 'degrees_north'
  #lat[:]=nodes['lat']

  nc.close()


