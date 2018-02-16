#! /bin/bash

# This script runs a suite of tests that check whether the getm subdomain
# decomposition and output can be correctly read in as input.  It
# relies on $MOSSCO_SETUPDIR/sns


mkdir -p stitched Parallel

ln -sf $MOSSCO_SETUPDIR/sns/Parallel/par_setup* Parallel/
ln -sf $MOSSCO_SETUPDIR/sns/getm.inp .

cat << EOT > mossco_run.nml
&mossco_run
 title = 'test_restart',
 start = '2013-07-01 00:00:00',
 stop = '2013-07-01 00:06:00',
/
EOT

mossco -bn0 test_restart

OWD=$(pwd)

for n in 12 16 22 32 61 83 96 117 144 158 178 192 288; do


mkdir -p $n; cd $n

cat << EOT > test_restart.yaml
coupling:
  - components:
     - getm
     - netcdf
    interval: 6 m
EOT


ln -sf $MOSSCO_SETUPDIR/sns/getm.inp .
grep 2013 $MOSSCO_SETUPDIR/sns/meteofiles.dat > meteofiles.dat
ln -sf $MOSSCO_SETUPDIR/sns/parallel.inp .
ln -sf $MOSSCO_SETUPDIR/sns/Topo/topo.nc .
ln -sf $MOSSCO_SETUPDIR/sns/bdy.2d.nc .
ln -sf $MOSSCO_SETUPDIR/sns/bdy.3d.nc .
ln -sf $MOSSCO_SETUPDIR/sns/sns_climatology.nc .
ln -sf $MOSSCO_SETUPDIR/sns/rivers.nc .
ln -sf $MOSSCO_SETUPDIR/sns/riverinfo.dat .
ln -sf $MOSSCO_SETUPDIR/sns/bdyinfo.dat .
ln -sf $MOSSCO_SETUPDIR/sns/gotmturb.nml
cp ../mossco_run.nml .
cp ../test_restart .

  cat << EOT > slurm_postprocess.sh
#!/bin/bash -x

#SBATCH --ntasks=1
#SBATCH --ntasks-per-core=1
#SBATCH --output=postprocess-%j.stdout
#SBATCH --error=postprocess-%j.stderr
#SBATCH --time=00:05:00
#SBATCH --partition=batch
#SBATCH --job-name=postprocess

srun ../postprocess.sh ${n}
EOT

  ln -sf ../Parallel/par_setup.${n}p*dat par_setup.dat

  mossco -n${n} -z 00:05:00 -p slurm_postprocess.sh test_restart

  cd $OWD
done
