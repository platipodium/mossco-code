#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

# ---------------------
# User configuration
# Declare a list of variables to extract
declare -a vn=("Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water" "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water" "Chl_chl_in_water" "Phytplankton_Carbon_phyC_in_water" "Zooplankton_Carbon_zooC_in_water")
# "detritus-P_in_soil" "mole_concentration_of_phosphate_in_soil" "Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water""Phytplankton_Carbon_phyC_in_water"  "Detritus_Carbon_detC_in_water" "fraction_of_Rubisco_Rub_in_water"  "Rubisco_fract._allocation__fracR_in_water" "mole_concentration_of_phosphate_in_soil"
#declare -a vn=("denitrification_rate_in_soil""fast_detritus_C_in_soil"  )
#declare -a vn=("mole_concentration_of_nitrate_in_soil" "phytoplankton_in_water" "nutrients_in_water"  "dissolved_oxygen_in_soil")

dnr=29
if [ $# -lt 1 ]; then
     # Where to get the extracted variables
  fname='mossco_gffrpr.'$dnr'.nc'  # Prefix of files to process
else
  fname=$1
fi
outdir='cut_'$dnr
mkdir -p $outdir

# build comma separated string
tg='time,doy,getmGrid2D_getm_lat,getmGrid2D_getm_lon,getmGrid3D_getm_lat,getmGrid3D_getm_lon,getmGrid2D_getm_Y,getmGrid2D_getm_X,getmGrid3D_getm_Y,getmGrid3D_getm_X'
ts=$model${vn[0]}
for (( i=1; i<${#vn[@]}; i++ )) do
  ts=$ts','${vn[$i]}
done # i
#echo $ts

if ! [[ -f $fname ]] ; then
   echo "File $fname does not exist"
   exit 1
fi
# loop over  coordinates around (Helgoland) station
for lon in 8 10 13; do
 for lat in 2 4; do
# lon=8 lat=3
  outname=$outdir'/cut_'$dnr'_'$lon$lat'.nc'
  echo $fname '->' $outname
# invokes nco tool and writes output to folder "cut/"
  ncks -O -v $tg,$ts\
    -d getmGrid3D_getm_1,$lon,$lon -d getmGrid3D_getm_2,$lat,$lat \
    -d getmGrid3D_getm_3,0,14,7 -d time,1,,1 $fname $outname
#		-d ungridded00015,0,0 \
		
#     ncap -O -s "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water=Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water-0.3*(1-exp(-Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water*3))- 1.* Phytplankton_Phosphorus_phyP_in_water"  $outname2 $outname
#   ncap -O -s "PC=Phytplankton_Phosphorus_phyP_in_water/(Phytplankton_Carbon_phyC_in_water+0.001)"  $outname $outname2
  done
done

