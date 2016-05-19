#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart fields from output files 
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

#ncpu=61
#ncpu=116ncpu=158
ncpu=178
fg=%03g

for p in $(seq -f $fg 0 1 $[$ncpu-1]); do ncks -O -d getmGrid3D_getm_3,0,5 -d getmGrid3D_getm_3,7,22,2 -d getmGrid3D_getm_3,24,29 'restart178Mar_30.'$p'.nc' 'restart'$ncpu'Mar_20.'$p'.nc' ; done

ncdump -h 'restart178Mar_20.001.nc' | head -20  
