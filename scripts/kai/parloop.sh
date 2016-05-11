#!/bin/bash
# scenario variation by kai wirtz HZG 2015
#  takes as optional argument the full path/name of mossco-executable
# Prog=$1 shift

model=maecs
export hd=${PWD}
rm -f tag.lst

#declare -a pnam=("phi_agg" "syn_nut" "PAds""rFast"  "rSlow" "NCrFdet" )
declare -a pnam=("a_minfr" "fT_exp_mort" "zm_fa_delmax" "mort_zoo" "zstoich_PN" "syn_nut" "decay_nut" "kwFzmaxMeth" "PAds")
declare -a pval=( 0.16       2.           6.             0.05        2.0        -6.          0.04       2    0.01 )

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
#for a in hr_*; do ~/kai/add_denit.sh $a/mossco_1d.nc; done
mkdir -p hr_res
for (( i=0; $i < ${#pnam[@]}; i++ )) do
 cp hr_$i/mossco_1d.nc hr_res/mossco_1d${pnam[$i]}.nc
done

