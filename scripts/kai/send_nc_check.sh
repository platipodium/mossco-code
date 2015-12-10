
aa=`/opt/netcdf/3.6.2/intel/bin/ncdump -v Chl_chl_in_water /data/wirtz/sns2/mossco_gffrn.21.nc | tail -3`
#aa=`/opt/netcdf/3.6.2/intel/bin/ncdump -v Phytplankton_Carbon_phyC_in_water /home/wirtz/NSBS153/mossco_gffrn.021.nc | tail -2`
mail -s ${aa:2:9} kai.wirtz@hzg.de < /dev/null

