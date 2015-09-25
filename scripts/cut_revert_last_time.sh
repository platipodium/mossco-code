#! /bin/bash

if ! test -f $1 ; then
  echo No file specified or file "$1" not found
  exit 1
fi

F=$1

# Find out the last time index
N=$(ncdump -h $F |grep '= UNLIMITED' |cut -f2 -d'(' |cut -f1 -d' ')
if [[ $N -gt 0 ]] ; then
  N=$(expr $N - 1)
fi

BASENAME=${F%%.*}
NAME=${F%%.nc}

if [[  ${NAME} -ne ${BASENAME} ]]; then
  INDEX=${NAME##${BASENAME}.}
  OUTNAME=${BASENAME}_$N.${INDEX}.nc
else
  OUTNAME=${BASENAME}_$N.nc
fi

rm -f _tmp.$F ${OUTNAME}
ncks -O -d time,$N  -v .*_in_soil,.*_in_water $F _tmp.$F
ncks -O -x -v .*sources-sinks.*,time _tmp.$F ${OUTNAME}
ncrename -v time,time1  ${OUTNAME}
echo ncap2 -A -s "time[time]=0.0*time1" ${OUTNAME} ${OUTNAME}
ncap2 -A -s "time[time]=0.0*time1" ${OUTNAME} ${OUTNAME}
