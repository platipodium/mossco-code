#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

dnr=68
if [ $# -lt 1 ]; then
     # Where to get the extracted variables
  fname='mossco_gffrpr.'$dnr'.nc'  # Prefix of files to process
else
  fname=$1
fi
outdir='cut_'$dnr
mkdir -p $outdir

if ! [[ -f $fname ]] ; then
   echo "File $fname does not exist"
   exit 1
fi
# loop over  coordinates around (Helgoland) station
for lon in 0 2; do
 for lat in 0 2; do
# lon=8 lat=3
  outname=$outdir'/cut_'$dnr'_'$lon$lat'.nc'
  echo $fname '->' $outname
# invokes nco tool and writes output to folder "cut/"
  ncks -O -v $tg,$ts\
    -d getmGrid3D_getm_1,$lon,$lon -d getmGrid3D_getm_2,$lat,$lat \
     $fname $outname
#		-d ungridded00015,0,0 \
#		-d getmGrid3D_getm_3,0,14,7 -d time,1,,1
  done
done

