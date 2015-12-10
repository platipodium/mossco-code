#!/bin/bash
#
# This script  is part of MOSSCO.  It tailors big netcdf files to relevant eco-variables
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Onur ....
# @author Kai W. Wirtz

#run by ssh wirtz@grpsrv09 "export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH; export PATH=/usr/local/bin/:$PATH; ~/ocean/home/postprocessing.sh $PWD cut" 
#aa=`pwd`; ssh wirtz@grpsrv09 "~/ocean/home/tools/postprocessing.sh $aa cut2"

if [[ "x$1" == "x" ]] ; then
  echo "This script reads the input and - in case- output folder as arguments."
  exit 1
fi

# short simulation name
simname='nsbs_ref_VarAtt'

# time stamp
ds=`date +"%H-%M_%d-%m-%y"`
echo "creating " $simname\_$ds.nc

# go to directory of full model output
echo $1
a=`basename $1`
echo "going to ~/ocean/data/"$a
cd ~/ocean/data/$a

~/ocean/home/tools/catnml $simname\_$ds.txt #store all setting in one txt file

if [[ "x$2" == "x" ]] ; then
  echo "set ~/ocean/data/cut as directory for reduced model output"
  cutd=~/ocean/data/cut 
else
# directory for reduced model output
  cutd=~/ocean/data/$2  
fi

~/ocean/home/tools/cut_ncks.sh 79 $cutd  #reduce full output


# go to directory of reduced model output
echo "going to " $cutd
cd $cutd
# invoke python-concatenating without argument
python ~/ocean/home/tools/stitch_tiles.py
# rename output in a recognizable, unique way
mv netcdf_out_stitched.nc $simname\_$ds.nc


