#!/bin/bash
# script to copy mossco-setup folders without result/log files
#  by kai wirtz HZG 2013

dir1=$1
dir2=$2
rn='slurm.sh'

mkdir -p $dir2
cd  $dir1

for a in `ls --ignore="mossco*.nc" --ignore="sns*.nc" --ignore="*.std*" --ignore="PET*"`; do echo $a; cp -d $a ../$dir2; done
cp -d sns_climatology.nc ../$dir2
cd  ../$dir2
ln -s ../$dir1/Topo
ln -s ../$dir1/Configurations
ln -s ../$dir1/Forcing
ln -s ../$dir1/restart
cp -rp ../$dir1/windmussel .
ln=`awk '/'job-name'/{print NR}' slurm.sh`
sed -i "$ln s/.*/#SBATCH --job-name=$dir2/" slurm.sh
cat slurm.sh

