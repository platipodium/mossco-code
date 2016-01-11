#!/bin/bash
#
# This script  is part of MOSSCO.  It fixes spatial coordinates from existing fixed coordinates
#
# @copyright (C) 2016 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#


if (( "$#" < 1));then
  echo "You must specify the input directory and prefix"
  exit 1
fi

if (( "$#" < 2));then
 fnameout=$(pwd)/mossco_gffrr
else
 fnameroot=$2
fi

latloncurv='getmGrid3D_getm_X,getmGrid3D_getm_Y,getmGrid2D_getm_X,getmGrid2D_getm_Y'

for F in ${fnamein}.*.nc; do
  echo ncks -O -v ${latloncurv} ${F} ${fnameout}${F#${fnamein}}
  ncks -O -v ${latloncurv} ${F} ${fnameout}${F#${fnamein}}
done
