#! /bin/bash

# @brief test routine for all generic couplings
#
# This computer program is part of MOSSCO. 
# @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>

#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

if [ "x$1" == "x" ]; then
  FILTER="*.yaml"
else
  FILTER="$1.yaml"
fi

MOSSCO_SETUPDIR=${MOSSCO_DIR%code}setups
#S=$MOSSCO_SETUPDIR/helgoland
S=$GETMDIR/../getm-setups/box_cartesian
L=${0%.sh}.log
G=$MOSSCO_DIR/examples/generic
P=$S/PET0.Helgoland
H=$(uname -mns)
D=$(date "+%Y%m%d %H:%M:00")
V=$(grep ESMF_LIBSDIR $ESMFMKFILE)
V=${V}

cat << EOT >> $L

Running tests on host $H with filter $FILTER on $D

Host system | configuration | date | Result
------------|---------------|------|--------
EOT


for F in $G/${FILTER}; do
  D=$(date "+%Y%m%d %H:%M:00")
  P=$S/PET0.Helgoland
  B=$(basename $F)
  B=${B%.yaml}
  python $G/create_coupling.py $F
  if [ $? -eq 0 ]; then :
  else
    echo "$H | $B | $D | python failed" >> $L
    continue
  fi
 
  if [ $(hostname) == ocean-fe.fzg.local ] ; then
    make -C $G all
  else
    make -j4 -C $G all
  fi
  if [ $? -eq 0 ]; then :
  else
    echo "$H | $B | $D | make failed" >> $L
    continue
  fi
  
  if [ $(hostname) == ocean-fe.fzg.local ] ; then
cat << EOT > $S/job.sh
#!/bin/sh

#$ -N  $B
#$ -pe orte 1
#$ -cwd
#$ -V

cat $PE_HOSTFILE
mpirun $G/coupling > $S/$B.log
EOT
    (cd $S; rm -f $P net*.nc; qsub $S/job.sh )
    while ! [ -f $P ] ; do
      sleep 1
    done

  else
    (cd $S ; rm -f PET* net*.nc ;  $G/coupling)
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
