#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart 1D fields from output files 
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz
fname=mossco_1d.nc
declare -a exnam=("rate" "flux" "velocity")

N=$(ncdump -h $fname |grep '= UNLIMITED' |cut -f2 -d'(' |cut -f1 -d' ')
N=$[$N-1]
ncks -O -d time,$N,$N $fname tmp.nc 

# delete irrelevant fields
for (( i=0; i<${#exnam[@]}; i++ )) do
 echo "remove " ${exnam[$i]}
 ncks -O -x -v '.*'${exnam[$i]}'*' tmp.nc tmp.nc
done

#"Chl_chl_in_water" "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"  "Phytplankton_Nitrogen_phyN_in_water"   "mole_concentration_of_nitrate_in_soil" "Detritus_Carbon_detC_in_water" 
echo "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water"
ncap -O -s "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water=0.+0.5*Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water)" tmp.nc tmp.nc

echo "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"
ncap -O -s "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water=5+1.7*Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water" tmp.nc tmp.nc

mv tmp.nc restart.nc
