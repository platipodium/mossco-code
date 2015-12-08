#!/bin/bash
# scenario variation by kai wirtz HZG 2015
#  takes as optional argument the full path/name of mossco-executable
# Prog=$1 shift

model=maecs
export hd=${PWD}
rm -f tag.lst

declare -a pnam=("PAdsODU" "mort_zoo" "fT_exp_mort" "g_max"  "rSlow" "NCrFdet")
declare -a pval=(  20         0.05           1.       0.5     0.006     0.18)

cd helgoland
echo "creating dirs ..." ${#pnam[@]}
~/mossco/scripts/create_hr_dirs.sh ${#pnam[@]}
ls ..
for (( i=0; $i < ${#pnam[@]}; i++ )) do
  echo "go to ..." $hd/hr_$i
  cd $hd/hr_$i
#   echo $i
#  for (( i=0; i<${#pnam[@]}; i++ )) do
  for a in *.nml
    do cat $a | grep "^[[:space:]]*${pnam[$i]}"
    if [ $? -eq 0 ]; then  fname=$a; fi;
  done
  echo "change " ${pnam[$i]} " from " $fname " to " ${pval[$i]} 
  replace  $fname ${pnam[$i]} ${pval[$i]}
  echo $i " " ${pnam[$i]} " " ${pval[$i]} >> ../tag.lst
  nohup ./mossco_1d & 
done # i
wait
cd $hd
for a in hr_*; do ~/mossco/scripts/add_denit.sh $a/mossco_1d.nc; done

