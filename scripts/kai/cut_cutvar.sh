#!/bin/bash
#
# This script is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz
# ---------------------
# User configuration
# Declare a list of variables to extract
tg='time,doy,getmGrid2D_getm_lat,getmGrid2D_getm_lon,getmGrid3D_getm_lat,getmGrid3D_getm_lon,getmGrid2D_getm_Y,getmGrid2D_getm_X,getmGrid3D_getm_Y,getmGrid3D_getm_X,dissolved_oxygen_upward_flux_at_soil_surface,dissolved_reduced_substances_upward_flux_at_soil_surface'

ncks -O -v $tg sns_z.nc sns_zv.nc

#declare -a vn=("Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water" "Chl_chl_in_water" "Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water"  "Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water"   "mole_concentration_of_nitrate_in_soil" "Detritus_Carbon_detC_in_water" "Phytplankton_Carbon_phyC_in_water" "Zooplankton_Carbon_zooC_in_water" 
# "Detritus_Phosphorus_detP_in_water""dissolved_oxygen_in_soil""layer_height_in_soil" "Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water" "fraction_of_Rubisco_Rub_in_water""detritus-P_in_soil" "temperature_in_water""Phytplankton_Phosphorus_phyP_in_water" "Phytplankton_Nitrogen_phyN_in_water"    "N\:C_ratio__QN_in_water" "P\:C_ratio__QP_in_water" "Rubisco_fract._allocation__fracR_in_water" "chlorophyll_to_carbon_ratio_in_water"  "denitrification_rate_in_soil" "mole_concentration_of_nitrate_in_soil  "mole_concentration_of_phosphate_in_soil"
# 'denitrification_rate_in_soil,layer_height_in_soil,fast_detritus_C_in_soil,detritus-P_in_soil,mole_concentration_of_nitrate_in_soil,mole_concentration_of_phosphate_in_soil,dissolved_oxygen_in_soil,dissolved_oxygen_upward_flux_at_soil_surface,dissolved_reduced_substances_upward_flux_at_soil_surface'