#!/bin/bash
#
# This script is part of MOSSCO.  It extracts restart fields from output files 
# @copyright (C) 2017 Helmholtz-Zentrum Geesthacht
# @author Kai W. Wirtz

cd /data/wirtz
cp tag.lst ~/sns/cut
#sns
for a in sns sns_*
 do 
 cd $a;
 ~/kai/cutavg_all.sh;
 cd ..;
done
cd ~/kai
matlab -nodisplay < plotgen.m > output.txt

cd ~/sns
ls -rtl
