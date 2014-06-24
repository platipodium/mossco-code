# -*- coding: utf-8 -*-
"""
Created on Fri Jun 20 10:49:26 2014

@author: Carsten Lemmen

This python script creates from a Triangle set of node and element files (.node and .ele)
a CF-convention unstructured grid (UGRID)
"""

from pylab import *
import netCDF4


def read_elements(basename):
    fid = open ( basename + '.ele', 'read')    

    rows = fid.readline().split()
    max_node_per_element = int(rows[1])
    
    i=0
    nodes=[]
    id=[]
    for line in fid.readlines():
      data = line.split()
      if data[0][0] == '#': break
      id.append(int(data[0]))
      nodes.append((int(data[1]), int(data[2]), int(data[3])))
      i=i+1
 
    fid.close()
    elements =  {'id': id, 'nodes' : nodes}   
    
    return elements, max_node_per_element

def read_nodes(basename):
    fid = open ( basename + '.node', 'read')    

    rows = fid.readline().split()
    
    i=0
    lon=[]
    lat=[]
    id=[]
    for line in fid.readlines():
      data = line.split()
      if data[0][0] == '#': break
      id.append(int(data[0]))
      lon.append(float(data[1]))
      lat.append(float(data[2]))
      i=i+1
 
    fid.close()
    nodes =  {'id': id, 'lat' : lat , 'lon': lon}   
    
    return nodes

      
       
if __name__ == '__main__':

  basename = '/Users/lemmen/devel/GIS/Triangle/topo.1'
  elements,max_node_per_element = read_elements(basename)  
  start_index=elements["id"][0]
  nodes = read_nodes(basename)  

  nc=netCDF4.Dataset(basename + '_ugrid.nc','w',format='NETCDF3_CLASSIC')

  nc.createDimension('node',len(nodes['id']))
  nc.createDimension('element',len(elements['id']))
  nc.createDimension('node_per_element',max_node_per_element)
  nc.createDimension('time',1)

  m = nc.createVariable('triangle_mesh','i4',())
  m.cf_role='mesh_topology'
  m.topology_dimension = 2.
  m.node_coordinates='lon lat'
  m.face_node_connectivity='nv'

  nv = nc.createVariable('nv','i',('element','node_per_element'))
  nv.start_index = start_index
  nv[:]=elements["nodes"]

  lon = nc.createVariable('lon','f8',('node',))
  lon.units = 'degrees_east'
  lon[:]=nodes['lon']

  lat = nc.createVariable('lat','f8',('node',))
  lat.units = 'degrees_north'
  lat[:]=nodes['lat']

  nc.close()


