#!/bin/bash

# script to store current nml files
# use: append_par.sh fname.nc
# authors: onur kerimoglu & kai wirtz HZG 2015

if (( "$#" > 0 ));then
 absname_ncf=$1
else
 absname_ncf=$PWD/mossco_gffn_xs.nc
fi
echo "handling nc file: $absname_ncf"

fname=$(basename $absname_ncf)
#extension="${fname##*.}"
fpath=$(dirname $absname_ncf)

ds=`date +"%H-%M_%m-%d-%y"`
#echo $ds

name_parf="${fname%.*}".par
echo "simulation date: $ds" > ${fpath}/${name_parf} 
cat maecs_*.nml fabm.nml fabm_pelagic.nml fabm_sed.nml run_sed.nml soil_pelagic_connector.nml >> ${fpath}/${name_parf}
echo "created parameter file: ${fpath}/$name_parf"

#strategy-1: write the name of the parameter file as an attribute
ncatted -a parameter_file,global,a,c,${name_parf} ${absname_ncf} #mossco_gffn_xs_rn.nc
ncatted -a simulation_date,global,a,c,${ds} ${absname_ncf}
echo "added parameter_file and simulation_date attributes to: ${absname_ncf}"

#strategy-2: add it as an attribute to a specified nc file (DOESN'T WORK)
#but first handle special characters
#sed -i 's/\[//g' ${name_parfile}  #replace [ with \[
#sed -i 's/\]//g' ${name_parfile}  #replace ] with \]
#sed -i 's/\-//g' ${name_parfile}  #replace ] with \]
#attstr="'"`cat ${name_parfile}`"'"
#ncatted -a parameters,global,o,c,${attstr} mossco_gffn_xs_rn.nc
#still some errors..

