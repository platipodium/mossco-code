#!/bin/sh

#file=mossco_gffrn
file=cut_
setup=sns
cdo griddes $file"00.nc" > $setup.griddes

cdo enlarge,$setup.griddes $file"00.nc" tmp.nc

cdo ifthenc,-9999. tmp.nc tmpA.nc
cdo ifnotthenc,-9999. tmpA.nc tmpB.nc

for p in $file*; do
cdo mergegrid tmpB.nc $p tmp0.nc
done

mv tmpB.nc $setup.nc
rm tmp*.nc
