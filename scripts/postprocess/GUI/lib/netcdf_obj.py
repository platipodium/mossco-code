# -----------------------------------------------------------------------------
# > @brief NCINFO - Visualization tool for NOSSCO-output files
#
#   This computer program is part of MOSSCO.
# > @copyright Copyright (C) 2013, 2014, 2015 Bundesanstalt fuer Wasserbau
# > @author Nikolai Chernikov, <nikolai.chernikov.ru@gmail.com>
#
#  MOSSCO is free software: you can redistribute it and/or modify it under the
#  terms of the GNU General Public License v3+.  MOSSCO is distributed in the
#  hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
#  LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
# ------------------------------------------------------------------------------

import sys
import os
import time
import shutil


class netCDFobj():
    def __init__(self, path, ncfile, create_tmp=False, log=False):
        self._workWithTmp = create_tmp
        self._loaded = self.init_me(path, ncfile, create_tmp=self._workWithTmp, log=log)
        print 'nc open'

    def init_me(self, path, ncfile, create_tmp=False, log=False):
        self._originFnameFullPath = os.path.join(path, ncfile)
        self._currentDir = os.path.dirname(sys.argv[0])
        self._tempFnameFullPath = os.path.join(self._currentDir, ncfile+'.tmp')
        
        if create_tmp:
            try:
                shutil.copy(self._originFnameFullPath, self._tempFnameFullPath)
                if log: print 'Copied data file to a temp file:\n\tdata: {0}\n\ttemp: {1}'.format(self._originFnameFullPath, self._tempFnameFullPath)
            except:
                err_msg = 'Cannot copy data file to a temp file:\n\tdata: {0}\n\ttemp: {1}'.format(self._originFnameFullPath, self._tempFnameFullPath)
                raise IOError (err_msg)
            #self.nc = netCDF4.Dataset(self._tempFnameFullPath, mode='r')
            return True
        
        else:  #work with original file
            #self.nc = netCDF4.Dataset(self._originFnameFullPath, mode='r')
            return True
    
    def get_ctime(self):
        # retreive file-creation time
        return time.ctime(os.path.getctime(self._originFnameFullPath))

    def get_fname(self):
        return self._originFnameFullPath, self._tempFnameFullPath

    def get_loadStatus(self):
        # retreive file-load status
        return self._loaded
    
    def close(self):
        print 'nc closed'
        #self.nc.close()  # close netcdf
        if os.path.isfile(self._tempFnameFullPath):
            os.remove(self._tempFnameFullPath)  # delete temp file
            print 'File removed: {0}'.format(self._tempFnameFullPath)
        del(self)
