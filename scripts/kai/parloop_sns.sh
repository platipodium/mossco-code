#!/bin/bash
# scenario variation by kai wirtz HZG 2015
#  takes as optional argument the full path/name of mossco-executable
# Prog=$1 shift

export hd=${PWD}
rm -f tag.lst

#declare -a pnam=("fT_exp_mort"  "PAds""rFast"  "rSlow" "NCrFdet" "syn_nut"vS_det""mort_zoo")zm_fa_inf remin "sinking_factor" "bioturbation"  "PAds" "PAdsODU" "rSlow""Q10""syn_nut"-4.5"zm_fa_delmax""a_water"
#declare -a pnam=( )# "vS_det" "QP_phy_max"  "fT_exp_mort" "a_minfr" "sinking_factor_min")
declare -a pnam=("ref"  "genMeth"  "genMeth" "mort_zoo" "a_minfr"  "a_chl" "a_fz" ) 
declare -a pval=( 0        17        0         0.017      0.05       0.005    6 )    

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
  qsub  sge.sh
#  qsub -q small.q sge.sh
  cd $hd
done # i
#wait
qstat
more tag.lst
for (( i=0; $i < ${#pnam[@]}; i++ )) do
  echo -n  "'${pnam[$i]}${pval[$i]}';"
done
