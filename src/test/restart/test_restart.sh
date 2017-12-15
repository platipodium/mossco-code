#! /bin/bash

# This script runs a suite of tests that check whether the getm subdomain
# decomposition and output can be correctly read in as input.  It
# relies on $MOSSCO_SETUPDIR/sns

cat << EOT > test_restart.yaml
coupling:
  - components:
     - getm
     - netcdf
EOT

cat << EOT > mossco_run.nml
&mossco_run
 title = 'test_restart',
 start = '2013-07-01 00:00:00',
 stop = '2013-07-01 10:00:00',
/
EOT

ln -sf $MOSSCO_SETUPDIR/sns/getm.inp .
grep 2013 $MOSSCO_SETUPDIR/sns/meteofiles.dat > meteofiles.dat
ln -sf $MOSSCO_SETUPDIR/sns/parallel.inp .
mkdir -p Parallel
ln -sf $MOSSCO_SETUPDIR/sns/Parallel/par_setup* Parallel/
ln -sf $MOSSCO_SETUPDIR/sns/Topo/topo.nc .
ln -sf $MOSSCO_SETUPDIR/sns/bdy.2d.nc .
ln -sf $MOSSCO_SETUPDIR/sns/bdy.3d.nc .
ln -sf $MOSSCO_SETUPDIR/sns/sns_climatology.nc .
ln -sf $MOSSCO_SETUPDIR/sns/rivers.nc .
ln -sf $MOSSCO_SETUPDIR/sns/riverinfo.dat .
ln -sf $MOSSCO_SETUPDIR/sns/bdyinfo.dat .
ln -sf $MOSSCO_SETUPDIR/sns/gotmturb.nml .

mossco -bn 1 test_restart

for n in 12 ; do #16 22 32 61 83 96 117 144 158 178 192 288; do

  ln -sf Parallel/par_setup.${n}p*dat par_setup.dat

  mossco -n${n} -b -z 00:05:00 test_restart
done
