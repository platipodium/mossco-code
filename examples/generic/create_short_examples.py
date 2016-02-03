#!/usr/bin/env python
# This script is is part of MOSSCO. It creates from YAML description of components
# and their one-letter abbreviations short links to all coupling specs.
#
# @copyright (C) 2016 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the

import sys
import os
import glob

try:
    import yaml
except:
    print('Please install the python-yaml package or set your PYTHONPATH variable\n')
    print('to the location of the python yaml package.')
    sys.exit(1)

filename = 'component_catalog.yaml'
fid = file(filename,'rU')
config = yaml.safe_load(fid)
fid.close()


files_to_process = glob.glob('*--*.yaml')

for i, filename in enumerate(files_to_process):
 
    basename = os.path.basename(os.path.splitext(filename)[0])
    print i, basename
    
    items = basename.split('--')
    shortname=''
    
    for j, item in enumerate(items):
        if config.has_key(item):
            shortname += config[item]
            
    if len(shortname) != j + 1: continue
    linkname=shortname + '.yaml'
        
    if os.path.isfile(linkname): os.remove(linkname)
        
    try:
      os.symlink(filename, linkname)
    except:
      print 'Some error occured when linking ' + linkname + ' to ' + filename
        
    
        
    
    
    