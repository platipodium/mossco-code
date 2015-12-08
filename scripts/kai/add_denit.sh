#!/bin/bash
#
fname=$1
ncap2 -O -s 'N2r=denitrification_rate_in_soil*layer_height_in_soil*porosity_in_soil'  $fname tmp.nc
ncap2 -O -s 'N2flux=N2r.total($ungridded00015)' tmp.nc $fname
ncatted -O -a coordinates,N2flux,m,c,"helgoland2d_lat helgoland2d_lon" $fname
rm tmp.nc