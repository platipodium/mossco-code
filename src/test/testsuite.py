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
import os, subprocess
import sys
import glob
import re
import datetime
import platform

def test_log(filename, name, value):
  fid=open(filename,'a')
  fid.write(name + ': ' + str(value) + '\n')       
  fid.close()
  
def test_make_targets(filename,code_dir,targets):
  for key,value in targets.iteritems():
    rc=os.system('cd ' + code_dir + '; make ' + value)
    test_log(test_result_name,'mk/'+key,True if rc == 0 else False)  
  
def test_make_subdirs(filename,code_dir,subdirs):
  for subdir in subdirs:
    rc=os.system('cd ' + code_dir + '; make -C ' + subdir)
    subdir=re.sub('examples/','ex/',subdir)
    test_log(test_result_name,'mk/' + subdir,True if rc == 0 else False)  

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

try:
  esmfmkfile=os.environ['ESMFMKFILE']
except:
  sys.exit('Fatal. Could not find environment variables $ESMFMKFILE')


now=str(datetime.datetime.utcnow().isoformat())[0:19]

test_script_name=sys.argv[0]
test_result_name=os.path.splitext(test_script_name)[0] + '_results_' \
 + re.split('\.',platform.node())[0] + '_' \
 + re.sub(' ','_',now) + '.txt'

fid=open(test_result_name,'w') 
fid.write('os/date: ' + now + '\n')
fid.write('os/host: ' +re.split('\.',platform.node())[0] + '\n')
fid.write('os/system: ' + platform.system() + '\n')
fid.write('os/version: ' + platform.version() + '\n')
fid.write('os/machine: ' + platform.machine() + '\n')

fid.close()

rc=os.system('cd ' + code_dir + '; git pull -u')
test_log(test_result_name,'git/pull',True if rc == 0 else False)

sha = subprocess.Popen("git log -n1 --oneline", shell=True, bufsize=1024,stdout=subprocess.PIPE).stdout.readline()
sha = re.split(' ',sha)[0]
test_log(test_result_name,'git/sha',sha)

rc=os.path.exists(esmfmkfile)
test_log(test_result_name,'esmf/esmf.mk',True if rc else False)

for line in open(esmfmkfile).readlines():
  if re.match('gotm', line): continue

# Define a list of make targets to pass
make_targets={#'clean':'mossco_clean', 
  'external':'external',
  'fabm':'libfabm_external', 'gotm':'libgotm_external',
  'getm':'libgetm_external', 
  #'doc': '-C doc'
}
test_make_targets(test_result_name,code_dir,make_targets)

make_subdirs=['src/utilities', 'src/drivers', 'src/connectors', 
'src/mediators','src/components', 'src']

test_make_subdirs(test_result_name,code_dir,make_subdirs)

make_examples=['standalone/omexdia_p','standalone/erosed',
  'standalone/benthos',
  'standalone/hamsom','standalone/tracer',
  'esmf/benthos','esmf/constant','esmf/getm','esmf/gotmfabm',
  'esmf/pelagicfabm1','esmf/sediment','esmf/clm','esmf/empty','esmf/fabm0d',
  'esmf/gotm','esmf/remtc','esmf/simplewave'
]

for i,example in enumerate(make_examples):
  make_examples[i] = os.path.join('examples/',example)

test_make_subdirs(test_result_name,code_dir,make_examples)
  
make_yamls=glob.glob(os.path.join(code_dir,'examples','generic','*.yaml'))

# Test generic yaml compilation
for filename in make_yamls:
  
  # Skip links to yamls
  if os.path.islink(filename): continue
  example=os.path.splitext(os.path.split(filename)[1])[0]

  # Skip yamls that already compiled successfully
  if os.path.isfile(os.path.splitext(filename)[0]): 
    test_log(test_result_name,'ex/g/'+example,True)
    continue       
  
  rc=os.system('cd ' + os.path.join(code_dir,'examples','generic') + '; python create_coupling.py ' + example + '; make')
  test_log(test_result_name,'ex/g/'+example,True if rc == 0 else False)
 
# Tests in deep_lake setup 
dl_dir=os.path.join(setup_dir,'deep_lake')

fid= open(os.path.join(dl_dir,'mossco_run.nml'),'w')
fid.write('''
&mossco_run
  title = "deep_lake-autmated-tests",
  start = "2002-01-02 00:00:00",
  stop= "2002-01-04 00:00:00",
  loglevel = "all",
  logflush = .true., /
''')
fid.close()

os.system('cd ' + dl_dir + '; git checkout -- getm.inp')

for yaml in glob.glob(os.path.join(code_dir,'examples','generic','*.yaml')):
  
  # Skip links to yamls
  if os.path.islink(yaml): continue
  
  # Exclude yamls that contain 1D gotm
  for line in open(yaml).readlines():
    if re.match('gotm', line): continue
        
  example=os.path.splitext(os.path.split(yaml)[1])[0]

  if not os.path.isfile(os.path.splitext(yaml)[0]): 
    test_log(test_result_name,'dl/g/'+example,False)
    continue   
    
  petname=os.path.join(dl_dir,'PET0.deep_lake-1x1-' + example)

  if  os.path.isfile(petname): os.unlink(petname)

  rc=os.system('cd ' + dl_dir + '; mossco -n1 -lA -sF ' + example)
  if rc != 0: 
    test_log(test_result_name,'dl/g/'+example,False)
    continue
      
  petname=os.path.join(dl_dir,'PET0.deep_lake-1x1-' + example)
  if not os.path.isfile(petname): 
    test_log(test_result_name,'dl/g/'+example,False)
    continue
      
  fid=open(os.path.join(dl_dir,'PET0.deep_lake-1x1-' + example),'r')
  lines=fid.readlines()
  
  try:
    lines[-1].index('finished')
  except:
    try:
      lines[-1].index('Finalize')
    except:
      test_log(test_result_name,'dl/g/'+example,False)
      continue
     
  test_log(test_result_name,'dl/g/'+example,True)

  for line in open(test_result_name).readlines():
    print line