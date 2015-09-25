#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
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
 fnameroot=mossco_gffrr
else
 fnameroot=$2
fi

# Determine number of processors to use
if (( "$#" >2 )) ; then
  n=$3
else
  n=4
fi

latlon='getmGrid3D_getm_lon,getmGrid3D_getm_lat,getmGrid2D_getm_lat,getmGrid2D_getm_lon'
latloncurv='getmGrid3D_getm_X,getmGrid3D_getm_Y,getmGrid2D_getm_X,getmGrid2D_getm_Y'
timedim='-d time,446,,3'
#vertdimW='-d getmGrid3D_getm_3,0,29,29'
#vertdimS='-d ungridded00024,0,4,4'

# Declare a list of variables to extract: 3D fields (Hvars), 4D water fields (Wvars) and 4D soil fields (Svars)
Hvars='water_depth_at_soil_surface'
Wvars='getmGrid3D_getm_layer,temperature_in_water,Detritus_Carbon_detC_in_water,Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water,Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water,Phytplankton_Carbon_phyC_in_water,Chl_chl_in_water'
#'concentration_of_SPM_in_water_001,concentration_of_SPM_in_water_002'
Svars='dissolved_oxygen_in_soil,denitrification_rate_in_soil,detritus-P_in_soil,mole_concentration_of_phosphate_in_soil,mole_concentration_of_ammonium_in_soil,mole_concentration_of_nitrate_in_soil'

#another way would be:
#declare -a vn=("pet_getmGrid2D_getm" "Photosynthetically_Active_Radiation_dPAR_in_water" "temperature_in_water")
# build comma separated string
#ts=''
#for (( i=0; i<${#vn[@]}; i++ )) do
#  ts=$ts','$model${vn[$i]}
#done # i

p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  G='cut.'$p'.nc'
  echo  "$F -> $G"
  #'lat-lon'
  ncks -O -v $latlon,$latloncurv $F $G &
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

echo -n "Processing soil 3D variables "
p=-1
for F  in $fnameroot.*.nc; do
  ((p++))
  wait ${pids[$p]}
  echo -n "."
done
echo " all done"

exit
