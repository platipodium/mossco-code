#!/bin/bash
# script to display selected parameters in mossco-setup folders 
#  by kai wirtz HZG 2015

# 130 34 r001 79 13 r002 113 20 r003 47 30 r004 8 11 r005 32 23 r006 22 15 r007 18 14 r008
lav=(34  13  20  30  11  23  15  14)
lov=(130 79 113  47   8  32  22  18)
# Detritus_Phosphorus_detP_flux_in_water
#Phytplankton_Phosphorus_phyP_flux_in_water
#Phytplankton_Nitrogen_phyN_flux_in_water
#Detritus_Nitrogen_detN_flux_in_water
#Chl_chl_flux_in_water
#Detritus_Carbon_detC_flux_in_water
#Phytplankton_Carbon_phyC_flux_in_water
#fraction_of_Rubisco_Rub_flux_in_water
Dissolved_Inorganic_Nitrogen_DIN_nutN_flux_in_water
for var in Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water
 do
 echo $var
 for ri in {0..7}
  do
  la=$[${lav[$ri]}-1]
  lo=$[${lov[$ri]}-1]
  echo $la $lo 
  ncks -s '%2.3f \n' -C -v $var -d lat,$la,$la -d lon,$lo,$lo river_grid_fluxes_phy.nc | less #| grep nan
#  ncra -y max -v $var -d lat,$la,$la -d lon,$lo,$lo river_grid_fluxes_phy.nc
 done 
done
