#! /bin/bash
#
# This script  is part of MOSSCO.  It attempts to call the postprocessing script correctly
# use: process_fabmout.sh mossco_gffn.00.nc
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @authors: onur.kerimoglu@hzg.de
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

outdir=/data/kerimogl/mossco-results/nsbs/nsbs47-3r
if (( "$#" > 0 ));then
  outdir=$1
fi
system=$2 #todo: doing it as a slurm or sge command on cluster

proc_script=$MOSSCO_DIR/scripts/postprocess/process_fabmout.sh
fnameroot=mossco_gffn
user=kerimogl
remotemachine=grpsrv09

#attempt to detect whether ncks is available locally
#if $(which ncks 2> /dev/null) ; then
if false ; then
	#do processing locally
	$proc_script $outdir $fnameroot $MOSSCO_DIR
else
	#do processing through a remote machine
        outdir=$(echo $outdir | sed 's/data/ocean-data/')
	MOSSCO_DIR=$(echo $MOSSCO_DIR | sed 's/home/ocean-home/')
        proc_script=$(echo $proc_script | sed 's/home/ocean-home/')
	echo '(remote) outdir:'$outdir
 	echo '(remote) proc_script:'$proc_script  	
	ssh ${user}@${remotemachine} "export LD_LIBRARY_PATH=/usr/local/lib:\$LD_LIBRARY_PATH; export PATH=/usr/local/bin/:\$PATH; $proc_script $outdir $fnameroot $MOSSCO_DIR" 
fi
