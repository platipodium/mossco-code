#!/usr/bin/env python
#> @brief Finds a tile in a multiprocessor output

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

def find_tile_for_position(topo_file, par_setup_file, position):
    
     if not os.path.exists(par_setup_file):
        print '$MOSSCO_SETUPDIR/sns/par_setup.dat cannot be found'
        return
        
     if not os.path.exists(topo_file):
        print '$MOSSCO_SETUPDIR/sns/par_setup.dat cannot be found'
        return
   
     nc=netCDF4.Dataset(topo_file,'r')
     latx = nc.variables['latx'][:]
     lonx = nc.variables['lonx'][:]
     
     latc = 0.5*(latx[0:-1,0:-1] + latx[1:,1:])
     lonc = 0.5*(lonx[0:-1,0:-1] + lonx[1:,1:])
     
     g_index=np.argmin((latc-position[1]) * (latc-position[1]) + (lonc-position[0]) * (lonc-position[0]))
     g_index=np.array(np.unravel_index(g_index, latc.shape))
     g_index=np.flip(g_index, axis=0)
    
     f = open(par_setup_file,'r')
     n = int(f.readline())
     tile_size = [int(x) for x in f.readline().split()]
     distribution = f.readlines()
     f.close()

     for line in distribution:
         if len(line)<2: continue
         i = [int(x) for x in line.split()]
         tile_id = i[0]
         ioff = i[1]
         joff = i[2]
         if tile_id == 0:
             global_index = g_index - [ioff, joff]
             
         tile_index = g_index - [ioff, joff]
         if np.all(tile_index > 0):
             if np.all(tile_index < tile_size[0:2]):
                 break

     return tile_id, tile_index, global_index, n
     
if __name__=='__main__':
    
    mossco_setup_dir = os.environ['MOSSCO_SETUPDIR']
    if not os.path.exists(mossco_setup_dir):
        print '$MOSSCO_SETUPDIR cannot be found'
        quit()
    
    if len(sys.argv)>1:
        n = sys.argv[1]
        #par_setup.144p.11x7.dat
    else:    
        par_setup_file = os.path.join(mossco_setup_dir,'sns','par_setup.dat')
    if not os.path.exists(par_setup_file):
        print '$MOSSCO_SETUPDIR/sns/par_setup.dat cannot be found'
        quit()

    topo_file = os.path.join(mossco_setup_dir,'sns','topo.nc')
    if not os.path.exists(topo_file):
        print '$MOSSCO_SETUPDIR/sns/topo.nc cannot be found'
        quit()

    position = (6.943,53.9538)
    [tile_id, tile_index, global_index,n] = find_tile_for_position(topo_file, par_setup_file, position)
    print tile_id, tile_index, global_index, n
    
    