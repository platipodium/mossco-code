NN=$1
sdir=${PWD##*/}
export NN
for (( p1=0; $p1 < $NN; p1++ )) do
  mkdir -p ../hr_$p1
  rsync -a *.* --exclude '*.nc' --exclude 'PET*' --exclude gotm_meteo.dat ../hr_$p1/
  rsync -a *.nc --exclude 'clm*helgoland.nc' --exclude helgoland.nc --exclude mossco_1d.nc ../hr_$p1/
  ln -s ../$sdir/gotm_meteo.dat ../hr_$p1/ 
  ln -s ../$sdir/mossco_1d ../hr_$p1/
  ln -s ../$sdir/clm.kse.2002-2005.1d.helgoland.nc ../hr_$p1/
  ln -s ../$sdir/clm.kse.2002-2005.2d.helgoland.nc ../hr_$p1/
done
