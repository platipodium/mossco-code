#!/bin/bash
# script to display selected parameters in mossco-setup folders 
#  by kai wirtz HZG 2015

dirs=$@   # all arguments: setup directories

#echo $dirs
for a in $dirs 
 do 
 printf "\n$a:  \n"
 cd $a
# cat getm.inp | grep "river_method ="
# cat getm.inp | grep "smooth_bvf_hor ="

 for par in a_water a_minfr small a_chl N_depo
   do
#   a=maecs_pars_$exp-qN.nml
   cat maecs_env.nml | grep "$par ="
   done
 for par in theta_LHC vS_det hydrol
   do
#   a=maecs_pars_$exp-qN.nml
   cat maecs_pars.nml | grep "$par ="
   done
 cd ..
done




