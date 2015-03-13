#! /bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
# use: process_fabmout.sh mossco_gffn.00.nc
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Onur Kerimoglu
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

toolpath=$MOSSCO_DIR/src/scripts/postprocess/
cutscript=$toolpath/cut_maecsomex.sh 
pystitchscript=$toolpath/stitch_tiles.py
pyplotscript=$toolpath/plot_mossco2Dmap.py

if (( "$#" > 0 ));then
 absname_ncf=$1
else
 absname_ncf=$PWD/mossco_gffn.00.nc
fi
fname=$(basename $absname_ncf)
fpath=$(dirname $absname_ncf)

fnameroot="${fname%.*.nc}" #mossco_gffn

#detect number of processors (=count of files that follows pattern fnameroot.*.nc)
ls -1 mossco_gffn.*.nc |wc -l > lines
nproc=`cat lines`
rm lines
#todo: more elegantly?

#extract
$cutscript $nproc $fnameroot
#echo 'cutting complete'

#stitch
fnameout=${fnameroot}'_xs.nc'
#fname='cut.'$p'.nc'
#ncmerge cut.*.nc $fnameout
python $pystitchscript cut 
mv 'cut_stitched.nc' $fnameout
#rm -r cut.*

#append parameters
${toolpath}append_par.sh $fnameout

#make #python $pyplotscript $fnameout
echo 'plotting variables:' $ts
python $pyplotscript $fnameout $ts 1,15

