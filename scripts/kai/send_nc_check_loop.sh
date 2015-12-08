cat /dev/null > tmp.log
short=""
file=mossco_gf*rr.29.nc
#Chl_chl_in_water Phytplankton_Carbon_phyC_in_water
for sd in sns sns_he sns_tke0 
 do
 aa=`/opt/netcdf/3.6.2/intel/bin/ncdump -v Chl_chl_in_water /data/wirtz/$sd/$file | tail -3`
 ti=`/opt/netcdf/3.6.2/intel/bin/ncdump -v doy /data/wirtz/$sd/$file | tail -2`
 echo $aa
 echo ${aa:3:6}
 short="$short:${aa:4:7}"
 echo $short
 echo -e '\n' $sd >> tmp.log
 echo $ti >> tmp.log
 echo -n ${aa:3:11} >> tmp.log
 done

mail -s \'$short\' kai.wirtz@hzg.de < tmp.log

