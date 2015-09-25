#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Onur Kerimoglu
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
 fnameroot=NSBS6nm.3d
else
 fnameroot=$2
fi

latlon='latc,lonc'
timedim='-d time,1,,5'
vertdim='-d level,1,30,29'

# Declare a list of variables to extract: 3D fields (Hvars), 4D water fields (Wvars) and 4D soil fields (Svars)
Wvars='salt,temp,h'


#another way would be:
#declare -a vn=("pet_getmGrid2D_getm" "Photosynthetically_Active_Radiation_dPAR_in_water" "temperature_in_water")
# build comma separated string
#ts=''
#for (( i=0; i<${#vn[@]}; i++ )) do
#  ts=$ts','$model${vn[$i]}
#done # i

p=-1
#for p in $(seq -f "%03g" 0 $(( nproc-1 ))); do
for F  in $fnameroot.*.nc; do
  ((p++))  
  #F=$fnameroot'.'$p.'nc'
  G='cut3D.'$p'.nc'
  echo "$F -> $G"
  #'lat-lon'
  ncks -O -v $latlon $F $G
  #3D vars
  ncks -A $timedim $vertdim -v ${Wvars} $F $G
done
exit
