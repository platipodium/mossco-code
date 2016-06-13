#!/bin/bash
# scenario variation by kai wirtz HZG 2015
#  takes as optional argument the full path/name of mossco-executable
# Prog=$1 shift

model=maecs
export hd=${PWD}
rm -f tag.lst

#declare -a pnam=("fT_exp_mort"  "PAds""rFast"  "rSlow" "NCrFdet" "syn_nut"vS_det""mort_zoo")zm_fa_inf remin "zm_fa_delmax"
#declare -a pnam=("Q10" "QP_phy_max" "syn_nut")
declare -a pnam=("Nqual" "vir_loss"  "vir_loss" "PAds"  "phi_agg" "phi_agg" "agg_doc"  "agg_doc" "rnit" "mort_zoo"  "mort_zoo" "AffP" "hydrol")
declare -a pval=(  0.     0.0        1.2          0        0.002     0.0004    0.01       0.2       150    0.015   0.025       0.05   0.02)

cd helgoland
echo "creating dirs ..." ${#pnam[@]}
~/kai/create_hr_dirs.sh ${#pnam[@]}
ls ..
for (( i=0; $i < ${#pnam[@]}; i++ )) do
  echo "'${pnam[$i]}${pval[$i]}';"
  echo "go to ..." $hd/hr_$i
  cd $hd/hr_$i
#   echo $i
#  for (( i=0; i<${#pnam[@]}; i++ )) do
  for a in `find *.nml ! -path "*omex*"`
    do cat $a | grep "^[[:space:]]*${pnam[$i]}"
    if [ $? -eq 0 ]; then  fname=$a; fi;
  done
  echo "change " ${pnam[$i]} " from " $fname " to " ${pval[$i]} 
  replace  $fname ${pnam[$i]} ${pval[$i]}
  echo $i " " ${pnam[$i]} " " ${pval[$i]} >> ../tag.lst
  nohup ./mossco_1d & 
  cd $hd
done # i
wait

#for a in hr_*; do ~/kai/add_denit.sh $a/mossco_1d.nc; done
mkdir -p hrres
for (( i=0; $i < ${#pnam[@]}; i++ )) do
 cp hr_$i/mossco_1d.nc hrres/mossco_1d${pnam[$i]}${pval[$i]}.nc

done
cd hrres
ln -s ../helgoland/mossco_1d_0.nc mossco_1dref.nc
cd ..
more tag.lst
