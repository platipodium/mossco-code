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
 title = 'test_regrid',
 start = '2013-07-01 00:00:00',
 stop = '2013-07-01 10:00:00',
/
EOT

ln -sf $MOSSCO_SETUPDIR/sns/getm.inp .
ln -sf $MOSSCO_SETUPDIR/sns/meteofile.dat .
ln -sf $MOSSCO_SETUPDIR/sns/parallel.inp .
mkdir -p Parallel
ln -sf $MOSSCO_SETUPDIR/sns/Parallel/par_setup* Parallel/

mossco -bn 1 test_restart

mossco -sF -n12 test_restart
