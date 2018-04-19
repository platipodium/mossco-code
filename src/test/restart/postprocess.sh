#!/bin/bash

for F in netcdf.*.nc; do
  ncks -O -d time,1 -v practical_salinity_in_water,getm* $F ${1}_${F}
done

python $MOSSCO_DIR/scripts/postprocess/stitch_tiles.py $1_netcdf
ncrename -v practical_salinity_in_water,salinity $1_netcdf.nc


