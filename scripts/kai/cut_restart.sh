#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart fields from output files 
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

ncpu=61
#ncpu=116ncpu=158
#ncpu=178
fg=%02g
declare -a exnam=("rate" "velocity")
nam='../mossco_gfbfrr.'
#mkdir -p $ncpu

echo 'retrieving last time step from ' $nam$[$ncpu -1]'.nc' 
N=$(ncdump -h $nam$[$ncpu -1]'.nc' |grep '= UNLIMITED' |cut -f2 -d'(' |cut -f1 -d' ')
N=$[$N -2]
N=1218
echo $N
for p in $(seq -f $fg 0 1 $[$ncpu-1]); do fname=$nam$p'.nc'; ncks -O -d time,$N,$N $fname tmp.$p.nc; done 

# delete irrelevant fields
for (( i=1; i<${#exnam[@]}; i++ )) do
 echo "remove " ${exnam[$i]}
 for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncks -O -x -v '.*'${exnam[$i]}'*' tmp.$p.nc tmp.$p.nc ; done
done

#"Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water" "Chl_chl_in_water" "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"  "Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water"   "mole_concentration_of_nitrate_in_soil" "Detritus_Carbon_detC_in_water" 

#echo "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water"
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water=0.1+1.15*Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water)" tmp.$p.nc tmp.$p.nc; done

#echo "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water=1+1.18*Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water" tmp.$p.nc tmp.$p.nc; done

#echo "Dissolved_Organic_Carbon_domC_in_water..."
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "Dissolved_Organic_Carbon_domC_in_water=2.5*Dissolved_Organic_Carbon_domC_in_water" tmp.$p.nc tmp.$p.nc; done

#echo "Zooplankton_Carbon_zooC_in_water..."
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "Zooplankton_Carbon_zooC_in_water=0.2+2*Zooplankton_Carbon_zooC_in_water" tmp.$p.nc tmp.$p.nc; done

#echo "detritus-P_in_soil..."
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "detritus-P_in_soil=1.7*detritus-P_in_soil" tmp.$p.nc tmp.$p.nc; done

#echo "mole_concentration_of_phosphate_in_soil..."
#for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "mole_concentration_of_phosphate_in_soil=1.7*mole_concentration_of_phosphate_in_soil" tmp.$p.nc tmp.$p.nc; done

for p in $(seq -f $fg 0 1 $[$ncpu-1]); do mv tmp.$p.nc 'restart'$ncpu'Jan_20.'$p'.nc' ; done
