#! /bin/bash

# @brief test routine for getm parallel couplings
# based on test_all_yaml.sh
#
# This computer program is part of MOSSCO. 
# @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
# @author Ulrich Körner <ulrich.koerner@hzg.de>

#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

# check parallel.inp (1-D, 2-D mesh), for a 2-D setup a value of 1 for NRPOCS_X/Y is not allowed
NPROCS_X=5
NPROCS_Y=2
((NPROCS=NPROCS_X*NPROCS_Y))
echo NPROCS $NPROCS

if [ "x$1" == "x" ]; then
  FILTER="*.yaml"
else
  FILTER="$1.yaml"
fi
FILTER=getm_coupling.yaml

exampleSetupName=box_cartesian
MOSSCO_SETUPDIR=${MOSSCO_DIR%code}setups
S=${GETM_SETUPDIR}/${exampleSetupName}
L=${0%.sh}.log
G=$MOSSCO_DIR/examples/generic
H=$(uname -mns)
D=$(date "+%Y%m%d %H:%M:00")

cat << EOT >> $L

Running tests on host $H with filter $FILTER on $D

Host system | configuration | date | Result
------------|---------------|------|--------
EOT


for F in $G/${FILTER}; do
  D=$(date "+%Y%m%d %H:%M:00")
  P=$S/PET0.${exampleSetupName}
  B=$(basename $F .yaml)

  make -C $MOSSCO_GETMDIR/src distclean
  make -C $S distclean
  make -C $G distclean

  python $G/create_coupling.py $F

  if [ $? -eq 0 ]; then :
  else
    echo "$H | $B | $D | python failed" >> $L
    continue
  fi
 
  # set up a dimension file for static compilation and set the link
  # a 2x2 setup corresponds to 4 process default setup
  sed -e "s/imin=1,imax=iextr/imin=1,imax=iextr\/$NPROCS_X/" \
      -e "s/jmin=1,jmax=jextr/jmin=1,jmax=jextr\/$NPROCS_Y/" \
      $S/box_cartesian.1p.dim > $S/box_cartesian.4p.dim
  ln -sf $S/box_cartesian.4p.dim $S/box_cartesian.dim
  ln -sf $S/box_cartesian.dim $GETMDIR/include/dimensions.h

  if [ $(hostname) == ocean-fe.fzg.local ] ; then
    make -C $MOSSCO_GETMDIR/src
    make -C $G all
  else
    make -j4 -C $G all
  fi
  if [ $? -eq 0 ]; then :
  else
    echo "$H | $B | $D | make failed" >> $L
    continue
  fi
  
  # remove files from last run
  rm -f $S/PET* $S/getm_coupling*
  
  # mossco_run.nml is no part of getm examples
  cat <<-EOT > $S/mossco_run.nml
	&mossco_run
	  title = '${exampleSetupName}'
	  start= "2004-01-01 00:00:00"
	  stop= "2004-01-11 00:00:00"
	/
	EOT

  # if getm model setup is set to serial, change it to parallel
  sed --in-place -e 's/parallel = .false./parallel = .true./' $S/getm.inp

  if [ $(hostname) == ocean-fe.fzg.local ] ; then

	cat <<-EOT > $S/job.sh
	#!/bin/sh

	#$ -N  $B
	#$ -pe orte $NPROCS
	#$ -cwd
	#$ -V

	cat \$PE_HOSTFILE
	mpirun -np $NPROCS $G/coupling > $S/$B.log
	EOT
    (cd $S; rm -f $P net*.nc; qsub $S/job.sh )
    while ! [ -f $P ] ; do
      sleep 1
    done

  else
    (cd $S ; rm -f $P net*.nc ;  $G/coupling)
    if [ $? -ne 0 ]; then
      echo "$H | $B | $D | run failed" >> $L
      continue
    fi
    if [ -f $P ]; then : 
    else
      echo "$H | $B | $D | failed, no PET.log $P" >> $L
      continue
    fi
  fi
  
exit
  
  mv $P ${P%.log}_$B.log
  P=${P%.log}_$B.log
  
  W=$(grep -c WARNING $P)
  E=$(grep -c ERROR $P)
  
  if [ $E -ne 0 ]; then
    echo "$H | $B | $D | failed, $E errors, $W warnings " >> $L  
    continue
  fi

  echo "$H | $B | $D | succeeded, $W warnings " >> $L   
  
  if [ $W -eq 0 ] ; then
    rm -f $P
  fi
  
done
