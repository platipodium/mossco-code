#!/bin/bash
# script to display selected parameters in mossco-setup folders 
#  by kai wirtz HZG 2015

dirs=$1   # 1st argument:	source directory

for a in $dirs
 do 
 cd $a; 
 mkdir -p cut
 ~/ocean/home/tools/cut_sns.sh 61 cut
 cd cut
 python ~/ocean/home/tools/stitch_sns.py 
 cd ../..
 done 
