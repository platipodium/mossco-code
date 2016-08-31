#!/bin/bash
# scenario variation by kai wirtz HZG 2015
#  takes as optional argument the full path/name of mossco-executable
# Prog=$1 shift

model=maecs
export hd=${PWD}
rm -f tag.lst

#declare -a pnam=("fT_exp_mort"  "PAds""rFast"  "rSlow" "NCrFdet"vS_det""mort_zoo")zm_fa_inf  "zm_fa_delmax"""rnit""PAds"
#declare -a pnam=("mort_zoo"  "mort_zoo" "zm_fa_delmax"  "V_NC_max" "rFast" "a_water" "syn_nut" "phi_agg"  "vir_infect" "vir_infect"    0.02      0.03            1           0.9     0.06       0.8      -4.6      0.003        0.001        0.0035  "vir_phyC" "vir_phyC"  "vir_spor_C" "vir_spor_C"  8.0          80       0.01           0.00001       
#declare -a pnam=("vir_mu"  "vir_mu" "vir_spor_r" "vir_spor_r" "vir_loss" "vir_loss" )
#declare -a pval=( 0.003      0.01      0.12       0.18      0.3     0.8 ) 
declare -a pnam=( "zm_fa_delmax" "zm_fa_delmax" "zm_fa_inf" "zm_fa_inf")
declare -a pval=(     1                9              0.01      2   )

#declare -a pnam=("alpha" "QN_phy_max" "QN_phy_max" "AffN" "AffP" "Nqual"  "agg_doc"  "agg_doc"  "syn_nut"  "hydrol" "remin"  "vS_phy" "vS_det" "QP_phy_max" "phi_agg" "phi_agg" )
#declare -a pval=(  0.22    0.17        0.26      0.15      0.05     0.4       0        0.01     -3.4        0.01    0.03       0.      30        0.006  0.001  0.01 )
#declare -a pnam=("vir_loss" "vir_loss" "vir_mu"  "vir_spor_r" "vir_spor_C" "vir_spor_C" "vir_phyC" "vir_phyC" "vS_phy" )
#declare -a pval=(    0.06      0.25      0.02     0.1      0.03          0.1          0.002      0.02           0.1    2.   0.02   0.1  2.0  )

cd hr
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
ln -s ../hr/mossco_1d_1.nc mossco_1dref.nc
cd ..
more tag.lst
