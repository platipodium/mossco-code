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
#SCRDIR=~/devel/MOSSCO/code/scripts/kai
SCRDIR=~/kai
#if [ $# -lt 1 ]; then
if [[ "x$1" == "x" ]] ; then
 nproc=144
# nproc=61
else
  nproc=$1
fi
prefix=mossco_gfbfrr.
#prefix=mossco_gw.
export prefix

outdir=${PWD##*/}    # simulation set-up folder
echo $outdir
if [[ $nproc -lt 100 ]]; then
  form="%02g"
  fname=${prefix}'29.nc'
else
  form="%03g"
  fname=${prefix}'029.nc'
fi
echo 'retrieve meta-info from ' $fname

# cd $outdir
$SCRDIR/catnml
# run cutting and averaging in parallel mode

# retrieve final time-step
N=$(ncdump -h $fname |grep '= UNLIMITED' |cut -f2 -d'(' |cut -f1 -d' ')
N=$[$N - 1]
#echo 'cutting until time step ' $N
##N=30
=======
#N=1218
echo 'cutting until time step ' $N

# here for 178-cpu setup using 6 processors; 
#for ((a=0;a<6;a++)); do $SCRDIR/cut_avg_benpel.sh $nproc /$HOME/sns/cut $a 6 $N & done
for ((a=0;a<6;a++)); do $SCRDIR/cut_avg_surf.sh $nproc $HOME/sns/cut $a 6 $N & done
#for ((a=0;a<6;a++)); do $SCRDIR/cut_avg_phygetm.sh $nproc cut $a 6 $N & done
wait
#check for completeness;

#if necessary re-run and/or re-process single domains: "~/tools/cut_avg.sh 56 cut 54 1 120"

#cd /scratch/wirtz/cut
cd $HOME/sns/cut
fname='*tmp*.nc'
rm $fname
fname='cut1_*.nc'
rm $fname
fname='cut2_*.nc'
rm $fname


# stitch the pieces
python  $SCRDIR/stitch_tiles.py cut_\*.nc $outdir'.nc'
#python  $SCRDIR/stitch_tiles.py cutz  # surface sums
#python  $SCRDIR/stitch_tiles.py cutm_\*.nc $outdir'_m.nc'  #monthly maximum
#python  $SCRDIR/stitch_tiles.py cutc_\*.nc $outdir'_chls.nc'  #subsurface chl
fname='cut*.nc'
rm $fname *.tmp
#fname='cutm_*.nc'
#rm $fname

# view results
##ncview $outdir'.nc' &
# ncview $outdir'_m.nc' &

#ls -lrt
# the difference between values in cutm and cut indicates temporal variability
