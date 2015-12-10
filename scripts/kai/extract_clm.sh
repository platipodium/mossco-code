#!/bin/bash
#
#original:
#ncrcat -d rlat,min_lat_index,max_lat_index -d rlon,min_lon_index,max_lon_index cDII.00.kss.*.nc  output.nc

inpdir='/ocean-data/trim/dat_atla_nordsee/CLM_HINDCAST'
outdir='/home/wirtz/ocean/data/clm'

do_extract=1
do_modify=1
yB=1951
yE=2010
#yE=1951
lons='-d rlon,39,74,1' #0,131
lats='-d rlat,50,85,1' #0,140
times='-d time,1,,6'
#times=''
vars='-v time,rlon,rlat,PMSL,U_10M,V_10M,T_2M,RELHUM_2M,CLCT,rotated_pole'

for y in $(seq $yB $yE); do
  fraw=${inpdir}/cDII.00.kss.$y.nc
  fcut=${outdir}/cDII.00.kss.$y.cut.nc
  fmod=${outdir}/cDII.00.kss.$y.cutmod.nc 
  ffin=${outdir}/cDII.00.kss.$y.getm-sns.nc 
  
  if [ "$do_extract" -eq "1" ]; then
    echo 'cut: ' $fraw '->' $fcut
    ncrcat -C $vars $lats $lons $times ${fraw} ${fcut}
  fi
  
  if [ $do_modify == "1" ]; then
    echo 'mod: ' $fcut '->' $fmod
    
    #get red of the height dimensions of length 1 
    ncwa -O -a height_2m,height_10m -d height_2m,0,0 -d height_10m,0,0 $fcut $fmod  
    ncks -x -v height_2m,height_10m $fmod $ffin
   
    #rename variables 
    ncrename -v PMSL,slp -v U_10M,u10 -v V_10M,v10 -v T_2M,t2 -v CLCT,tcc -v RELHUM_2M,rh ${ffin} 
    ncrename -d rlon,lon -v rlon,lon -d rlat,lat -v rlat,lat ${ffin}
    #ncks -x -v 0m $fmod $ffin
    
    #calculate td from rh&t
    #dev2=243.04*(LN(RH/100)+((17.625*T)/(243.04+T)))/(17.625-LN(RH/100)-((17.625*T)/(243.04+T))) 
    #ncap -s "dev2-" $fmod ${fmod}_dev.nc    
    
    #rm $fcut $fmod
  fi
done


