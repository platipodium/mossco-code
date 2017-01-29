#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart fields from output files 
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz
if [[ "x$2" == "x" ]] ; then
  echo "This script needs names of input and output file as argument."
  exit 1
else
  infile=$1 
  outfile=$2

#cdo setmisstoc,0 -setmissval,-999 $infile tmp.nc

#declare -a exnam=("rate" "flux" "velocity")
ncap2 -O -s "Zooplankton_Carbon_zooC_in_water=hzg_maecs_nutN; fraction_of_Rubisco_Rub_in_water=hzg_maecs_nutN; Phytplankton_Carbon_phyC_in_water=hzg_maecs_nutN; Phytplankton_Phosphorus_phyP_in_water=hzg_maecs_nutN; Phytplankton_Nitrogen_phyN_in_water=hzg_maecs_nutN; Chl_chl_in_water=hzg_maecs_nutN; Virus_C_density_in_cells_vir_in_water=hzg_maecs_nutN;" $infile tmp.nc
ncap2 -O -s "where(hzg_maecs_nutN>0) {Zooplankton_Carbon_zooC_in_water=2.; fraction_of_Rubisco_Rub_in_water=1-0.05*hzg_maecs_nutN; Phytplankton_Carbon_phyC_in_water=7-0.35*hzg_maecs_nutN; Phytplankton_Phosphorus_phyP_in_water=0.07-0.0035*hzg_maecs_nutN; Phytplankton_Nitrogen_phyN_in_water=1-0.05*hzg_maecs_nutN; Chl_chl_in_water=1-0.05*hzg_maecs_nutN; Virus_C_density_in_cells_vir_in_water=0.1-0.005*hzg_maecs_nutN; }" tmp.nc $outfile

#cdo setmisstoc,-2E20 -setmissval,-2E20 tmp.nc $outfile

fi
