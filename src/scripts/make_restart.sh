#! /bin/bash

if ! test -f $1 ; then
  No file specified or file "$1" not found
  exit 1
fi

F=$1

BASENAME=${F%%.*}
NAME=${F%%.nc}
INDEX=${NAME##${BASENAME}.}
CORRNAME=/ocean-data/lemmen/mossco-setups/coordinates/mossco_gffn.${INDEX}.nc

echo $BASENAME $NAME $INDEX

N=$(ncdump -h $F |grep '= UNLIMITED' |cut -f2 -d'(' |cut -f1 -d' ')

if [[ $N -gt 0 ]] ; then
  N=$(expr $N - 1)
fi

OUTNAME=${BASENAME}_$N.${INDEX}.nc

rm -f _tmp.$F ${OUTNAME}
ncks -O -d time,$N  -v .*_in_soil,.*_in_water $F _tmp.$F
ncks -O -x -v .*sources-sinks.*,time _tmp.$F ${OUTNAME}
ncrename -v time,time1  ${OUTNAME}
echo ncap2 -A -s "time[time]=0.0*time1" ${OUTNAME} ${OUTNAME}
ncap2 -A -s "time[time]=0.0*time1" ${OUTNAME} ${OUTNAME}
ncks -A -v getmGrid2D_getm_lat,getmGrid2D_getm_lon ${CORRNAME} ${OUTNAME}
ncap2 -A -s "getmGrid3D_getm_lon[getmGrid2D_getm_1]=getmGrid2D_getm_lon" ${OUTNAME}
ncap2 -A -s "getmGrid3D_getm_lat[getmGrid2D_getm_2]=getmGrid2D_getm_lat" ${OUTNAME}
