#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

#for p in $(seq -f "%03g" 0 1 $ncpu); do fname='../sns_max/mossco_gffrr.'$p'.nc'; $ncpuks -O -d time,955,955 $fname restart0.$p.nc; done 
#for p in $(seq -f "%03g" 0 1 $ncpu); do ncap -s "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water=Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water*(0.9-Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water/(150+Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water))" restart0.$p.nc restart1.$p.nc; done
ncpu=60
#ncpu=116

for p in $(seq -f "%02g" 0 1 $ncpu); do fname='../../sns_fP/mossco_gffrr.'$p'.nc'; ncks -O -d time,179,179 $fname tmp0.$p.nc; done 
echo "Detritus_Carbon_detC_in_water=0.3"
echo "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"

for p in $(seq -f "%02g" 0 1 $ncpu); do ncap -O -s "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water=5+0.7*Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water" tmp0.$p.nc restart15N.$p.nc; done


for p in $(seq -f "%02g" 0 1 $ncpu); do ncap -O -s "Detritus_Carbon_detC_in_water=2.0+3*Detritus_Carbon_detC_in_water" tmp0.$p.nc restart15D.$p.nc; done

echo "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water"
for p in $(seq -f "%03g" 0 1 $ncpu); do ncap -O -s "Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water=0.2+1.2*Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water)" tmp0.$p.nc tmp1.$p.nc; done

echo "fraction_of_Rubisco_Rub_in_water=0.3+Detritus_Nitrogen_detN_in_water"
for p in $(seq -f "%03g" 0 1 $ncpu); do ncap -O -s "fraction_of_Rubisco_Rub_in_water=0.3+Detritus_Nitrogen_detN_in_water" tmp0.$p.nc tmp1.$p.nc; done
echo "Phytplankton_Nitrogen_phyN_in_water=0.1"
for p in $(seq -f "%03g" 0 1 $ncpu); do ncap -O -s "Phytplankton_Nitrogen_phyN_in_water=0.1+Detritus_Nitrogen_detN_in_water" tmp1.$p.nc tmp0.$p.nc; done

echo "Phytplankton_Carbon_phyC_in_water=1.0"
for p in $(seq -f "%03g" 0 1 $ncpu); do ncap -O -s "Phytplankton_Carbon_phyC_in_water=1.0+5*Detritus_Nitrogen_detN_in_water" tmp0.$p.nc tmp1.$p.nc; done

echo "Phytplankton_Phosphorus_phyP_in_water=0.01"
for p in $(seq -f "%03g" 0 1 $ncpu); do ncap -O -s "Phytplankton_Phosphorus_phyP_in_water=0.01+0.1*Detritus_Nitrogen_detN_in_water" tmp1.$p.nc tmp0.$p.nc; done
echo "Chl_chl_in_water=0.3"
for p in $(seq -f "%03g" 0 1 $ncpu); do ncap -O -s "Chl_chl_in_water=0.3+Detritus_Nitrogen_detN_in_water" tmp0.$p.nc restart1.$p.nc; done


