#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart fields from output files 
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

ncpu=61
#ncpu=116ncpu=158
#ncpu=178
fg=%02g

#"Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water" "Chl_chl_in_water" "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"  "Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water"   "mole_concentration_of_nitrate_in_soil" "Detritus_Carbon_detC_in_water" 

#echo "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water"
for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "Chl_chl_in_water=0.3*Phytplankton_Carbon_phyC_in_water)" 'restart'$ncpu'Feb_20.'$p'.nc' 'restart'$ncpu'Feb_a0_20.'$p'.nc'; done
for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncap -O -s "fraction_of_Rubisco_Rub_in_water=0.3*Phytplankton_Carbon_phyC_in_water)" 'restart'$ncpu'Feb_a0_20.'$p'.nc' 'restart'$ncpu'Feb_a0_20.'$p'.nc'; done
