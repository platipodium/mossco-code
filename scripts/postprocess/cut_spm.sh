#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant spm variables
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Onur Kerimoglu
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

if (( "$#" < 1 ));then
 print 'error: specify number of nodes'
else
 nproc=$1
fi

if (( "$#" < 2));then
 fnameroot=mossco_gfsen
else
 fnameroot=$2
fi

# Determine number of processors to use
if (( "$#" >2 )) ; then
  n=$3
else
  n=4
fi

#timedim='-d time,1,,2'
vertdimW='-d getmGrid3D_getm_3,0,29,29'
#vertdimS='-d ungridded00024,0,4,4'
latlon='getmGrid3D_getm_lon,getmGrid3D_getm_lat,getmGrid2D_getm_lat,getmGrid2D_getm_lon'

# Declare a list of variables to extract: 3D fields (Hvars), 4D water fields (Wvars) and 4D soil fields (Svars)
Hvars='water_depth_at_soil_surface,depth_averaged_x_velocity_in_water,depth_averaged_y_velocity_in_water,depth_averaged_y_velocity_in_water'

Wvars='getmGrid3D_getm_layer,concentration_of_SPM_in_water_001,concentration_of_SPM_in_water_002'

p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  G='cut.'$p'.nc'
  echo  "$F -> $G"
  #'lat-lon'
  ncks -O -v $latlon $F $G &
  pids[$p]=$!
  if [[ $(expr $p % $n) == $(expr $n - 1) ]] ; then
    wait ${pids[$p]}
  fi
done

echo -n "Processing water dimensions "

p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  wait ${pids[$p]}
  echo -n .
done
echo " done"

p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  G='cut.'$p'.nc'
  #2D vars
  ncks -A $timedim -v ${Hvars} $F $G &
  pids[$p]=$!
  if [[ $(expr $p % $n) == $(expr $n - 1) ]] ; then
    echo "  waiting for $G (pid  ${pids[$p]})"
    wait ${pids[$p]}
  fi
done

echo -n "Processing water 2D variables "
p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  wait ${pids[$p]}
  echo -n .
done
echo " done"

p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  G='cut.'$p'.nc'
  #3D vars in water
  ncks -A $timedim $vertdimW -v ${Wvars} $F $G &
  pids[$p]=$!
  if [[ $(expr $p % $n) == $(expr $n - 1) ]] ; then
    wait ${pids[$p]}
  fi
done

echo -n "Processing water 3D variables "
p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  wait ${pids[$p]}
  echo -n .
done
echo "done"

p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  G='cut.'$p'.nc'
  #3D vars in soil
  ncks -A $timedim $vertdimS -v ${Svars} $F $G &
  pids[$p]=$!
  if [[ $(expr $p % $n) == $(expr $n - 1) ]] ; then
    wait ${pids[$p]}
  fi
done

echo " all done"

exit

