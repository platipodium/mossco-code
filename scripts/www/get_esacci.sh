#!/bin/bash
#
# systematic harvest of opendap via ncss service
# by Carsten Lemmen

dest='esacci'
mkdir -p $dest

var='chlor_a'
#var='kd_490'
south=50
north=58
west=0
east=10

srv='https://rsg.pml.ac.uk/thredds/ncss/CCI_ALL-v2.0-DAILY'

# loop over all years of interest
for (( y=2003; $y < 2014; y++ ));  do

  yy=$(printf '%04g' $y)

  urlConst='disableProjSubset=on&addLatLon=true'
  urlBound='&north='$north'&east='$east'&south='$south'&west='$west
  urlVar='&var='$var'&accept=netCDF'
  urlTime='&time_start='$yy'-01-01T00:00:00Z&time_end='$yy'-12-31T23:59:59Z'

    oname=$var'_0010_5058_'$yy'_daily.nc'
    #echo wget -O $oname '"'$srv'?'$urlConst$urlBound$urlVar$urlTime'"'
    wget -O $dest/$oname "${srv}?${urlConst}${urlBound}${urlVar}${urlTime}"
done # y

# Get the 5-daily time into 1 file
srv='https://rsg.pml.ac.uk/thredds/ncss/CCI_ALL-v2.0-5DAY'
urlTime='&time_start=2003-01-01T00:00:00Z&time_end=2013-12-31T23:59:59Z'
oname=$var'_0010_5058_2003_2013_5day.nc'

wget -O $dest/$oname "${srv}?${urlConst}${urlBound}${urlVar}${urlTime}"

# Get the 8-daily time into 1 file
srv='https://rsg.pml.ac.uk/thredds/ncss/CCI_ALL-v2.0-8DAY'
urlTime='&time_start=2003-01-01T00:00:00Z&time_end=2013-12-31T23:59:59Z'
oname=$var'_0010_5058_2003_2013_8day.nc'

wget -O $dest/$oname "${srv}?${urlConst}${urlBound}${urlVar}${urlTime}"

# Get the monthly time into 1 file
srv='https://rsg.pml.ac.uk/thredds/ncss/CCI_ALL-v2.0-MONTHLY'
urlTime='&time_start=2003-01-01T00:00:00Z&time_end=2013-12-31T23:59:59Z'
oname=$var'_0010_5058_2003_2013_monthly.nc'

wget -O $dest/$oname "${srv}?${urlConst}${urlBound}${urlVar}${urlTime}"
