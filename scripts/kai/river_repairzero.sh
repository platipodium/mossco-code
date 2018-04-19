#!/bin/bash
#
# This script is part of MOSSCO. 
# It corrects zero data in discharge files 
# @copyright (C) 2016 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

if [ $# -lt 1 ]; then
     # or
  fname='river_grid_fluxes.nc'  # Prefix of files to process
else
  fname=$1
fi

if ! [[ -f $fname ]] ; then
   echo "File $fname does not exist"
   exit 1
fi

ncap2 -O -s "volume_flux_in_water=volume_flux_in_water+50*exp(-volume_flux_in_water)" $fname $fname



