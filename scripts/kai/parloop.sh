#!/bin/bash
# scenario variation by kai wirtz HZG 2015
#  takes as optional argument the full path/name of mossco-executable
# Prog=$1 shift

model=maecs
export hd=${PWD}
rm -f tag.lst

declare -a pnam=("kwFzmaxMeth" "N_depo" "mort_ODU" "hydrol" "fT_exp_mort" "rnit" "kinO2denit"  )
declare -a pval=(      3         0.12       1.      0.012       2.5          2      50   )

cd helgoland
echo "creating dirs ..." ${#pnam[@]}
~/kai/create_hr_dirs.sh ${#pnam[@]}
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
for a in hr_*; do ~/kai/add_denit.sh $a/mossco_1d.nc; done

