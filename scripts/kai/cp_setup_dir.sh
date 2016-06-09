#!/bin/bash
# script to copy mossco-setup folders without result/log files
#  by kai wirtz HZG 2013

dir1=$1
dir2=$2
#rn=slurm.sh
#tag='#SBATCH --job-name='
#tags=job-name
rn=sge.sh
tag='#$ -N '
tags='-N'

mkdir -p $dir2
cd  $dir1

for a in `ls --ignore="mossco*.nc" --ignore="sns*.nc" --ignore="silt*.nc" --ignore="boundary*.nc"--ignore="restart.00*" --ignore="*.std*" --ignore="PET*"`; do cp -d $a ../$dir2; done
cp -d sns_climatology.nc ../$dir2
cp -rp Configuration ../$dir2
cd  ../$dir2
ln -s ../$dir1/Topo
#ln -s ../$dir1/Configurations
ln -s ../$dir1/Forcing
ln -s ../$dir1/restart
ln -s ../$dir1/Parallel
#ln -s ../$dir1/windmussel 
#cp -rp ../$dir1/windmussel .

ln=`awk '/'${tags}'/{print NR}' ${rn}`
echo 'ln='$ln

sed -i "$ln s/.*/${tag}${dir2}/" ${rn}
cat ${rn}

