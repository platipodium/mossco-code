#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart fields from output files 
# @copyright (C) 2016 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

if [ $# -lt 1 ]; then
     # or
  fname='river_grid_fluxes.nc'  # Prefix of files to process
else
  fname=$1
fi

if ! [[ -f $fname ]] ; then
   echo "File $fname does not exist"
   exit 1
fi

rP=0.3
rN=0.3
nname=$(basename $fname .nc)'_rN'$rN'rP'$rP'.nc'

# "Chl_chl_in_water" "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"  "Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water"  

echo "Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water * " $rP
echo "Dissolved_Inorganic_Nitrogen_DIN_nutN_flux_in_water * "  $rN
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do 
#ncap2 -O -s "Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water='$rP'*Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water" $fname tmp.nc

ncap2 -O -s "where(Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water>10) {Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water=${rP}*Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water;Dissolved_Inorganic_Nitrogen_DIN_nutN_flux_in_water=${rN}*Dissolved_Inorganic_Nitrogen_DIN_nutN_flux_in_water; }" $fname $nname



