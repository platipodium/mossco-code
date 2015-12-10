#!/bin/bash
#
# This script is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

# edit  cut_avg.sh (e.g. variable, names, vertical sclicing,..)

if [ $# -lt 1 ]; then
  outdir=sns    # simulation set-up folder
else
  outdir=$1
fi

cd $outdir

# run cutting and averaging in parallel mode

# retrieve final time-step
N=$(ncdump -h mossco_gffrr.29.nc |grep '= UNLIMITED' |cut -f2 -d'(' |cut -f1 -d' ')
N=$[$N -1]
#N=100

# here for 61-cpu setup using 6 processors; 
for ((a=0;a<6;a++)); do ~/devel/MOSSCO/scripts/cut_sns.sh 178 cut $a 6 $N & done

wait
#check for completeness;

#if necessary re-run and/or re-process single domains: "~/tools/cut_avg.sh 56 cut 54 1 120"

cd cut
fname='*tmp*.nc'
rm $fname
fname='cut1_*.nc'
rm $fname
fname='cut2_*.nc'
rm $fname

# stitch the pieces
python ~/devel/MOSSCO/scripts/stitch_tiles.py cut
#python ~/tools/stitch_tiles.py cutz  # surface sums
#python ~/tools/stitch_tiles.py cutm  #monthly maximum

# view results
mv cut_stitched.nc 'all_'$outdir'.nc'
#mv cutm_stitched.nc $outdir'_m.nc'
#mv cutz_stitched.nc $outdir'_z.nc'
ncview $outdir'.nc' &
# ncview $outdir'_m.nc' &

ls -lrt
# the difference between values in cutm and cut indicates temporal variability


