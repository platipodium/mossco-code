#! /bin/bash

T="$1"
S=../coordinates/mossco_gffn

for F in $(ls cut*nc) ; do
	test -f $F || continue

  B=${F%%.nc}
  I=${B##*.}
  B=${B%%.*}

  ncks -A -c -v getmGrid2D_getm_lat,getmGrid2D_getm_lon ${S}.${I}.nc $F
  ncap2  -A -s "getmGrid3D_getm_lat=getmGrid2D_getm_lat" $F $F
  ncap2  -A -s "getmGrid3D_getm_lon=getmGrid2D_getm_lon" $F $F

  echo $F

done



