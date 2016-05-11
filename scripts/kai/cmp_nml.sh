cmpdir=$1

for a in maecs_*.nml fabm_*.nml run_sed.nml *_connector.nml *_input.cfg
 do
 diff -w -y --suppress-common-lines $a $cmpdir/$a
 if [ $? -eq 0 ]
 then
  echo -n ""
#  echo "identical $a"
 else
  sdiff -o $a.tmp -W --suppress-common-lines $a $cmpdir/$a
  echo -e "\t\t*****  $a  *****"
 fi
done
