# -*- coding: utf-8 -*-
"""
# This script is is part of MOSSCO. It is an automated test suite
# for updating, compiling, and running MOSSCO examples
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
"""

import os, sys, glob, re

try:
  code_dir=os.environ['MOSSCO_DIR']
except:
  sys.exit('Fatal. Could not find environment variables $MOSSCO_DIR and $MOSSCO_SETUPDIR')


test_dir=os.path.join(code_dir,'src','test')
test_results=glob.glob(os.path.join(test_dir,'testsuite_results_*:*.txt'))

# Read all test result file
results=[]
tests={}
testlist=[]

for i,filename in enumerate(test_results):
  mytests={}
  last_index=-1

  stamp=(os.path.splitext(os.path.split(filename)[1])[0])[18:]  
  
  for line in open(filename,'r').readlines():
    key=re.split(':',line)[0]
    value=re.split(':',line)[1]
    mytests[key]=value

    if key in testlist: 
      last_index=testlist.index(key)
    else:
      testlist.insert(last_index+1,key)
      last_index=testlist.index(key)
     
    if not tests.has_key(key):
      tests[key]={stamp: value}
    else:
      tests[key][stamp]=value
      
  results.append(mytests)

fid = open(os.path.join(test_dir,'testsuite_results_all.md'),'w')
fid.write('\# | Test ')
for j, result in enumerate(results): 
  fid.write(' | ' + str(j))
fid.write('\n --- | ---')
for j, result in enumerate(results):
  fid.write(' | ---')
fid.write('\n')

for i, test in enumerate(testlist):
  fid.write(str(i) + ' | ' + test  )
  for j, result in enumerate(results):
    if results[j].has_key(test):
      res=re.sub('\n','',results[j][test]).strip()
      if res=='False': 
        res="<font color='red'>fail</font>"
      elif res=='True':
        res="<font color='green'>pass</font>"    
      fid.write(' | ' + res)
    else:
      fid.write(' | - ')
      
  fid.write('\n')
    
fid.close()


  
    