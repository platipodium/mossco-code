#!/usr/bin/env python
#> @brief Creates a transect from mossco output along given input coordinates

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2017 Helmholtz Zentrum Geesthacht
#> @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file

import os
import sys
import netCDF4
import numpy as np
import find_tile

def create_var(ncin,ncout,varname,dims):

    value = ncin[varname]

    try:
        var=ncout.createVariable(varname,value.dtype,tuple(dims),fill_value=value.getncattr('_FillValue'))
    except:
        var=ncout.createVariable(varname,value.dtype,tuple(dims))

    for att in value.ncattrs():
        if att == '_FillValue': continue
        var.setncattr(att,value.getncattr(att))

    #print 'Created for output variable ', varname , tuple(dims)

def create_dim(ncin,ncout,dimname):


    if dimname.startswith('getmGrid') and (('D_2' in dimname) or \
        ('D_1' in dimname)): return


    try:
        if dimname == 'time' :
            ncout.createDimension(dimname, None)
        else:
            ncout.createDimension(dimname, len(ncin.dimensions[dimname]))
    except: pass


def copy_station_from_file(ncout,tile_file,tile_index,n,istation):

    try:
        ncin = netCDF4.Dataset(tile_file, 'r')
    except:
        ncin.close()
        ncin = netCDF4.Dataset(tile_file, 'r')

    try:
        ncout.createDimension('station',n)
    except: pass

    if 'index_j' not in ncout.variables:
        ncout.createVariable('index_j','i4',('station'))
    if 'index_i' not in ncout.variables:
        ncout.createVariable('index_i','i4',('station'))
    ncout.variables['index_j'][istation] =  tile_index[1]
    ncout.variables['index_i'][istation] =  tile_index[0]

    for varname,value in ncin.variables.iteritems():

        indims = value.dimensions
        outdims=[]

        for dimname in indims:

            if dimname.startswith('getmGrid') and (('D_2' in dimname) or  ('D_1' in dimname)):

                if  not 'station' in outdims:
                    outdims.append('station')

            else:
                outdims.append(dimname)
                create_dim(ncin,ncout,dimname)


        if varname not in ncout.variables:

            create_var(ncin,ncout,varname,outdims)

        if not 'station' in outdims:
            ncout.variables[varname][:] = value[:]
            continue

        if len(outdims) == 1:
            if len(indims) == 1:
                if 'D_2' in dimname:
                    ncout.variables[varname][istation] = value[tile_index[1]]
                else: ncout.variables[varname][istation] = value[tile_index[0]]
            else: ncout.variables[varname][istation] = value[tile_index[1],tile_index[0]]
        elif len(outdims) == 2:
            if 'station' == outdims[1]:
                ncout.variables[varname][:,istation] = value[:,tile_index[1],tile_index[0]]
            else:
               ncout.variables[varname][istation,:] = value[tile_index[1],tile_index[0],:]
        elif len(outdims) == 3:
            if 'station' == outdims[2]:
                ncout.variables[varname][:,:,istation] = value[:,:,tile_index[1],tile_index[0]]
            elif 'station' == outdims[1]:
                ncout.variables[varname][:,istation,:] = value[:,tile_index[1],tile_index[0],:]
            else:
               ncout.variables[varname][istation,:,:] = value[tile_index[1],tile_index[0],:,:]

        else: print 'Not implemented: ',indims,outdims


    ncin.close()

def create_transect_for_positions(results_file,positions):

    if not os.path.exists(results_file):
        print results_file + ' cannot be found'
        return

    s = results_file.split('.')
    outfile = s[0] + '_transect.nc'

    ncout = netCDF4.Dataset(outfile, 'w', format='NETCDF4_CLASSIC')

    n=len(positions)
    for i, position in enumerate(positions):

        copy_station_from_file(ncout,results_file,list(position),n,i)
        #print i, position

    ncout.close()
    print 'Results saved in ' + outfile

def create_transect_for_positions_tiled(results_file,positions):

    if not os.path.exists(results_file):
        print results_file + ' cannot be found'
        return

    mossco_setup_dir = os.environ['MOSSCO_SETUPDIR']
    if not os.path.exists(mossco_setup_dir):
        print '$MOSSCO_SETUPDIR cannot be found'
        quit()

    par_setup_file = os.path.join(mossco_setup_dir,'sns','par_setup.dat')
    if not os.path.exists(par_setup_file):
        print '$MOSSCO_SETUPDIR/sns/par_setup.dat cannot be found'
        quit()

    topo_file = os.path.join(mossco_setup_dir,'sns','topo.nc')
    if not os.path.exists(topo_file):
        print '$MOSSCO_SETUPDIR/sns/topo.nc cannot be found'
        quit()

    s = results_file.split('.')
    outfile = s[0] + '_transect.nc'

    ncout = netCDF4.Dataset(outfile, 'w', format='NETCDF4_CLASSIC')

    tiles = {}
    for i,position in enumerate(positions):

        tile_id,tile_index,global_index,n = find_tile.find_tile_for_position(topo_file, par_setup_file, position)
        tile_str = str(tile_id)
        print tile_id

        if np.any(tile_index < 0):
           print tile_id,tile_index,global_index,n
           continue

        if tiles.has_key(tile_str):
            tiles[tile_str].append(list(tile_index))
        else:
            tiles[tile_str]= [list(tile_index)]

        s = results_file.split('.')
        tile_file = '{prefix}.{number:02d}.nc'.format(prefix=s[0],number=tile_id)

        try:
            copy_station_from_file(ncout,tile_file,tile_index,n,i)
        except:pass

    ncout.close()
    print 'Results saved in ' + outfile

if __name__=='__main__':

    #positions = [(8.6,54.10), (8.4,54.10), (8.2,54.12), (8.0,54.15), (7.45,54.17)]
    lats = [53.871429,54.175406,54.120620]
    lons = [8.713617,7.895426,8.859637]
    n=40
    pos = np.zeros((2*n,2),dtype=float)
    pos[0:n,0] = np.linspace(lons[0],lons[1],n)
    pos[n:,0] = np.linspace(lons[1],lons[2],n)
    pos[0:n,1] = np.linspace(lats[0],lats[1],n)
    pos[n:,1] = np.linspace(lats[1],lats[2],n)


    positions=map(tuple, pos)


    results_file = '/Users/lemmen/devel/mossco/setups/sns/mossco_highres.06.nc'
    results_file = '/Volumes/Kea/data/MOSSCO/voynova/summer_spm.00.nc'

    create_transect_for_positions_tiled(results_file,positions)
