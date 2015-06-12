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
from datetime import datetime

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
if rc != 0:
  tests['git']=True

rc=os.system('cd ' + code_dir + '; make mossco_clean')
if rc != 0:
  tests['clean']=True

rc=os.system('cd ' + code_dir + '; make external')
if rc != 0:
  tests['external']=True

rc=os.system('cd ' + code_dir + '; make -C src/utilities')
if rc != 0:
  tests['utilities']=True

rc=os.system('cd ' + code_dir + '; make libfabm_external')
if rc != 0:
  tests['fabm']=True
  
  
   