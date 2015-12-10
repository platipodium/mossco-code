#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

# ---------------------
# User configuration

n1=1
dn=30

if [ $# -gt 0 ]; then
  n1=$1
fi
if [ $# -gt 1 ]; then
  dn=$2
fi

echo $n1 $dn
# loop over periods
for (( i=0; i<$n1; i+=$dn )) do
  i1=$[$i+$dn-1]
  if [ $i1 -gt $[$n1-1] ]; then
    i1=$[$n1-1]
  fi 
  echo $i $i1
  python ~/ocean/home/tools/plot.py $i-$i1 &
done # i
echo $ts

