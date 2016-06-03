
for a in sns_*; do cd $a; echo $a; ncks -F -s "%1.3f " -v Chl_chl_in_water -d getmGrid3D_getm_1,7 -d getmGrid3D_getm_2,7 -d getmGrid3D_getm_3,29 mossco_gfbfrr.29.nc | tail -4 | head -1; cd ..; done

