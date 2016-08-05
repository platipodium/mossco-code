#!/bin/bash
#
# systematic harvest of opendap data
# by kai wirtz HZG 2016
# and Carsten Lemmen
# lists of things to get

declare -a month_len=(0 31 28 31 30 31 30 31 31 30 31 30 31)

dest='esacci/'
mkdir -p $dest

srv='https://rsg.pml.ac.uk/thredds/ncss/CCI_ALL-v2.0-DAILY'

# loop over all years/months of interest
# time starts as daily integer count from 1 Jan 1970
mkdir -p $dest
for (( y=2003; $y < 2014; y++ ));  do

  yy=$(printf '%04g' $y)

  for m in $(seq 1 1 12); do

    mm=$(printf %02g $m)

    urlConst='disableProjSubset=on&addLatLon=true'
    urlBound='&north=58&east=10&south=50&west=0'
    urlVar='&var=chlor_a&accept=netCDF'
    urlTime='&time_start='$yy'-'$mm'-01T00:00:00Z&time_end='$yy'-'$mm'-'${month_len[$m]}'T23:59:59Z'

    oname='chl_'$yy$mm'_daily.nc'
    #echo wget -O $oname '"'$srv'?'$urlConst$urlBound$urlVar$urlTime'"'
    wget -O $oname $srv'?'$urlConst$urlBound$urlVar$urlTime
  done # m
done # y
