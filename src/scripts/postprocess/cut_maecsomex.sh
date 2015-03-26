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
 fnameroot=mossco_gffn
else
 fnameroot=$2
fi

latlon='getmGrid3D_getm_lon,getmGrid3D_getm_lat,getmGrid2D_getm_lat,getmGrid2D_getm_lon'
timedim='-d time,1,,2'
vertdimW='-d getmGrid3D_getm_3,0,29,29'
vertdimS='-d ungridded00024,0,4,4'

# Declare a list of variables to extract: 3D fields (Hvars), 4D water fields (Wvars) and 4D soil fields (Svars)
Hvars='water_depth_at_soil_surface,depth_averaged_x_velocity_in_water,depth_averaged_y_velocity_in_water,dissolved_oxygen_upward_flux_at_soil_surface,dissolved_reduced_substances_upward_flux_at_soil_surface,fast_detritus_C_upward_flux_at_soil_surface' # wave_height,
Wvars='getmGrid3D_getm_layer,temperature_in_water,Detritus_Carbon_detC_in_water,Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water,Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water,Phytplankton_Carbon_phyC_in_water,Zooplankton_Carbon_zooC_in_water,Chl_chl_in_water'
#'concentration_of_SPM_in_water_001,concentration_of_SPM_in_water_002'
Svars='dissolved_oxygen_in_soil,dissolved_reduced_substances_in_soil,denitrification_rate_in_soil,fast_detritus_C_in_soil,slow_detritus_C_in_soil,detritus-P_in_soil,mole_concentration_of_phosphate_in_soil,mole_concentration_of_ammonium_in_soil,mole_concentration_of_nitrate_in_soil'

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
  G='cut.'$p'.nc'
  echo "$F -> $G"
  #'lat-lon'
  ncks -O -v $latlon $F $G
  #2D vars
  ncks -A $timedim -v ${Hvars} $F $G
  #3D vars in water   
  ncks -A $timedim $vertdimW -v ${Wvars} $F $G    
  #3D vars in soil   
  ncks -A $timedim $vertdimS -v ${Svars} $F $G    
done
exit
