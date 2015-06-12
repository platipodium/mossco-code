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
import os
import sys
import glob

try:
  code_dir=os.environ['MOSSCO_DIR']
  setup_dir=os.environ['MOSSCO_SETUPDIR']
except:
  sys.exit('Fatal. Could not find environment variables $MOSSCO_DIR and $MOSSCO_SETUPDIR')

code_dir=code_dir + '-automated'
if not os.path.exists(code_dir):
  rc=os.system('git clone git://git.code.sf.net/p/mossco/code ' + code_dir)
  if rc != 0:
    sys.exit('Fatal. Could not git clone mossco.')
   
tests={}

rc=os.system('cd ' + code_dir + '; git pull -u')
tests['git']=True if rc == 0 else False

try:
  esmfmkfile=os.environ['ESMFMKFILE']
except:
  sys.exit('Fatal. Could not find environment variables $ESMFMKFILE')

rc=os.path.exists(esmfmkfile)
tests['esmf.mk']=True if rc else False

# Define a list of make targets to pass
test_make_targets={'clean':'mossco_clean', 
  'external':'external', 'utilities':'-C src/utilities',
  'fabm':'libfabm_external', 'gotm':'libgotm_external',
  'getm':'libgetm_external', 'drivers':'-C src/drivers',
  'connectors':'-C src/connectors', 'mediators':'-C src/mediators',
  'components':'-C src/components', 'src': '-C src', 
  #'doc': '-C doc'
}

test_make_examples=['standalone/omexdia_p','standalon/erosed',
  'standalone/benthos',
  'standalone/hamsom','standalone/tracer',
  'esmf/benthos	','esmf/constant','esmf/getm','esmf/gotmfabm',
  'esmf/pelagicfabm1','esmf/sediment','esmf/clm','esmf/empty','esmf/fabm0d',
  'esmf/gotm','esmf/remtc','esmf/simplewave'
]


for key,value in test_make_targets.iteritems():
  rc=os.system('cd ' + code_dir + '; make ' + value)
  tests[key]=True if rc == 0 else False
    
for item in test_make_examples:
  rc=os.system('cd ' + code_dir + '; make -C ' + os.path.join('examples',item))
  tests['ex/'+ item]=True if rc == 0 else False
  
for filename in glob.glob(os.path.join(code_dir,'examples','generic','*.yaml')):
  
  # Skip links to yamls
  if os.path.islink(filename): continue
  example=os.path.splitext(os.path.split(filename)[1])[0]

  # Skip yamls that already compiled successfully
  if os.path.isfile(os.path.splitext(filename)[0]): 
    tests['ex/g/'+ example]=True
    continue       
  
  rc=os.system('cd ' + os.path.join(code_dir,'examples','generic') + '; python create_coupling.py ' + example + '; make')
  tests['ex/g/'+ example]=True if rc == 0 else False
  
dl_examples=[]  


for key,value in tests.iteritems():
  print key,'=',value
   