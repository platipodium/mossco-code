#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart fields from output files 
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

#ncpu=61
#ncpu=116ncpu=158
ncpu=178
fg=%03g
declare -a exnam=("rate" "flux" "velocity")

#mkdir -p $ncpu

for p in $(seq -f $fg 0 1 $[$ncpu-1]); do fname='../mossco_gfbfrr.'$p'.nc'; ncks -O -d time,360,360 $fname tmp.$p.nc; done 

# delete irrelevant fields
for (( i=1; i<${#exnam[@]}; i++ )) do
 echo "remove " ${exnam[$i]}
 for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncks -O -x -v '.*'${exnam[$i]}'*' tmp.$p.nc tmp.$p.nc ; done
done

#"Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water" "Chl_chl_in_water" "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"  "Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water"   "mole_concentration_of_nitrate_in_soil" "Detritus_Carbon_detC_in_water" 
#echo "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water"
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water=0.+0.75*Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water)" tmp.$p.nc tmp.$p.nc; done

#echo "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water=0+0.75*Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water" tmp.$p.nc tmp.$p.nc; done

for p in $(seq -f $fg 0 1 $[$ncpu-1]); do mv tmp.$p.nc 'restart'$ncpu'Sep_30.'$p'.nc' ; done
