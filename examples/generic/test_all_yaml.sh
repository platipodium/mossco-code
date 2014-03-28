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

MOSSCO_SETUPDIR=$MOSSCO_DIR/../mossco-setups
S=$MOSSCO_SETUPDIR/helgoland
L=${0%.sh}.log
G=$MOSSCO_DIR/examples/generic
P=$S/PET0.Helgoland
H=$(uname -mns)
D=$(date "+%Y%m%d %H:%M:00")

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
  
  make -j2 -C $G all
  if [ $? -eq 0 ]; then :
  else
    echo "$H | $B | $D | make failed" >> $L
    continue
  fi
    
  (cd $S ; rm -f PET* net*.nc ;  $G/coupling)
  if [ $? -ne 0 ]; then
    echo "$H | $B | $D | run failed" >> $L
    continue
  fi
  
  if [ -e $P ]; then : 
  else
    echo "$H | $B | $D | failed, no PET.log $P" >> $L
    continue
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
