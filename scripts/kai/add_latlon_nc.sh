#!/bin/bash

fname=$1

ncrename -v chl-a_hplc,chl_hplc $fname
ncrename -v chl-a,chl -v copepods,zooC -v PO4,DIP $fname
ncap2 -O -s "PhyC=diatoms+flagellates" $fname $fname

#ncatted --attribute lat,lat,a,c,54.18 $fname
#ncatted --attribute lat,lon,a,c,54.18 $fname
#ncatted --attribute lon,lat,a,c,7.9 $fname
#ncatted --attribute lon,lon,a,c,7.9 $fname

for v in chl zooC DIN DIP PhyC ; do

ncrename -v $v,Helgoland-$v $fname


done
ncatted --attribute lat,,a,d,54.18 $fname
ncatted --attribute lon,,a,d,7.9 $fname



