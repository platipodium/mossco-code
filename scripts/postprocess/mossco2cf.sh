#!/bin/bash 

# This script converts MOSSCO output to CF conventions

test -f $1 || exit
F=$1

echo "Trying to make $F CF-compliant"

ncatted -a Conventions,global,o,c,'CF-1.6' $F
ncatted -a standard_name,getmGrid3D_getm_lat,o,c,'latitude' $F
ncatted -a long_name,getmGrid3D_getm_lat,o,c,'Latitude' $F
ncatted -a formula_terms,getmGrid3D_getm_lat,d,c,'Latitude' $F
ncatted -a missing_value,getmGrid3D_getm_lat,d,c,'Latitude' $F

ncatted -a long_name,getmGrid2D_getm_lat,o,c,'Latitude' $F
ncatted -a standard_name,getmGrid2D_getm_lat,o,c,'latitude' $F
ncatted -a formula_terms,getmGrid2D_getm_lat,d,c,'Latitude' $F
ncatted -a missing_value,getmGrid2D_getm_lat,d,c,'Latitude' $F

ncatted -a standard_name,getmGrid3D_getm_lon,o,c,'longitude' $F
ncatted -a long_name,getmGrid3D_getm_lon,o,c,'Longitude' $F
ncatted -a formula_terms,getmGrid3D_getm_lon,d,c,'Latitude' $F
ncatted -a missing_value,getmGrid3D_getm_lon,d,c,'Latitude' $F

ncatted -a long_name,getmGrid2D_getm_lon,o,c,'Longitude' $F
ncatted -a standard_name,getmGrid2D_getm_lon,o,c,'longitude' $F
ncatted -a formula_terms,getmGrid2D_getm_lon,d,c,'Latitude' $F
ncatted -a missing_value,getmGrid2D_getm_lon,d,c,'Latitude' $F

ncatted -a long_name,getmGrid2D_getm_X,o,c,'X axis' $F
ncatted -a units,getmGrid2D_getm_X,o,c,'m' $F
ncatted -a standard_name,getmGrid2D_getm_X,o,c,'projection_x_coordinate' $F
ncatted -a missing_value,getmGrid2D_getm_X,d,c,'Latitude' $F

ncatted -a units,getmGrid3D_getm_X,o,c,'m' $F
ncatted -a long_name,getmGrid3D_getm_X,o,c,'X axis' $F
ncatted -a standard_name,getmGrid3D_getm_X,o,c,'projection_x_coordinate' $F
ncatted -a missing_value,getmGrid3D_getm_X,d,c,'Latitude' $F

ncatted -a long_name,getmGrid2D_getm_Y,o,c,'Y axis' $F
ncatted -a units,getmGrid2D_getm_Y,o,c,'m' $F
ncatted -a standard_name,getmGrid2D_getm_Y,o,c,'projection_y_coordinate' $F
ncatted -a missing_value,getmGrid2D_getm_Y,d,c,'Latitude' $F

ncatted -a long_name,getmGrid3D_getm_Y,o,c,'Y axis' $F
ncatted -a units,getmGrid3D_getm_Y,o,c,'m' $F
ncatted -a standard_name,getmGrid3D_getm_Y,o,c,'projection_y_coordinate' $F
ncatted -a missing_value,getmGrid3D_getm_Y,d,c,'Latitude' $F

ncatted -a calendar,time,o,c,'gregorian' $F

ncatted -a standard_name,water_depth_at_soil_surface,o,c,'sea_floor_depth_below_sea_surface' $F
ncatted -a coordinates,water_depth_at_soil_surface,o,c,'getmGrid2D_getm_lon getmGrid2D_getm_lat' $F

ncatted -a standard_name,dissolved_oxygen_upward_flux_at_soil_surface,o,c,'surface_downward_mole_flux_of_molecular_oxygen' $F
ncatted -a units,dissolved_oxygen_upward_flux_at_soil_surface,o,c,'mol m-2 s-1' $F
ncatted -a coordinates,dissolved_oxygen_upward_flux_at_soil_surface,o,c,'getmGrid2D_getm_lon getmGrid2D_getm_lat' $F

ncflint -O -C -v dissolved_oxygen_upward_flux_at_soil_surface -w -0.001,0.0 $F $F temp.nc
ncks -A -v dissolved_oxygen_upward_flux_at_soil_surface temp.nc $F
rm -f temp.nc

#ncks -v dissolved_oxygen_upward_flux_at_soil_surface,water_depth_at_soil_surface $F temp.nc
#mv temp.nc $F 

ncatted -a history,global,o,c,'Very long file manipulation history removed' $F
