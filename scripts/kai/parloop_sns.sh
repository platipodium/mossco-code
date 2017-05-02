#!/bin/bash
# scenario variation by kai wirtz HZG 2015
#  takes as optional argument the full path/name of mossco-executable
# Prog=$1 shift

export hd=${PWD}
rm -f tag.lst

#declare -a pnam=("fT_exp_mort"  "PAds""rFast"  "rSlow" "NCrFdet""vS_det"  "syn_nut"mort_zoo")zm_fa_inf remin "sinking_factor" "bioturbation"  "PAds" "PAdsODU" "rSlow""Q10"-4.5"zm_fa_delmax""a_water""PAdsODU"  "a_water" "syn_nut"  "Q10"rFast
#declare -a pnam=( )# "QP_phy_max"  "fT_exp_mort" "a_minfr")"ref""vS_det""mort_zoo""PAds" 
declare -a pnam=("mort_zoo" "mort_zoo" "vir_mu"  "a_water" "mort_ODU" "mort_ODU"  )
declare -a pval=(  0.05     0.03       -0.05        1.2        -24      -32 ) 

for (( i=0; $i < ${#pnam[@]}; i++ )) do
  echo "'${pnam[$i]}${pval[$i]}';"
  echo "copy sns setup to " ${pnam[$i]}${pval[$i]}
  ~/kai/cp_setup_dir.sh sns sns_${pnam[$i]}${pval[$i]}
  cd sns_${pnam[$i]}${pval[$i]}
#   echo $i
#  for (( i=0; i<${#pnam[@]}; i++ )) do
  fname=''
  for a in `find *.nml ! -path "*omex*"`
    do cat $a | grep "^[[:space:]]*${pnam[$i]}"
    if [ $? -eq 0 ]; then  fname=$a; fi;
  done
  echo "change " ${pnam[$i]} " from " $fname " to " ${pval[$i]} 
  replace  $fname ${pnam[$i]} ${pval[$i]}
  echo $i " " ${pnam[$i]} " " ${pval[$i]} >> $hd/tag.lst
  if [ "${pnam[$i]}" == "sinking_factor_min" ]; then
    sf=$(echo "scale=2;(0.4-${pval[$i]})/0.6" | bc)
    echo "change sinking_factor from " $fname " to "  $sf
    replace  $fname sinking_factor $sf
  fi
  if [ "${pnam[$i]}" == "vS_det" ]; then
    sf=$(echo "scale=2;350-(${pval[$i]}-21)*10" | bc)
    echo "change sinking_factor from " $fname " to "  $sf
    replace fabm_sed.nml rnit $sf
  fi
  if [ $i -gt 6 ]; then  
   qsub  sge.sh
  else
   qsub  sge.sh
#   qsub -q small.q sge.sh
  fi
  cd $hd
done # i
#wait
qstat
more tag.lst
for (( i=0; $i < ${#pnam[@]}; i++ )) do
  echo -n  "'_${pnam[$i]}${pval[$i]}';"
done
