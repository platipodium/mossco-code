#!/usr/bin/env python
#> @brief Script to check flux input

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2015 Helmholtz Zentrum Geesthacht
#> @author Onur Kerimoglu <onur.kerimoglu@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
#!/usr/bin/env python
#> @brief Script to check an .nc file created by create_river_grid.

from copy import deepcopy
from collections import OrderedDict
import netCDF4
import netcdftime
import numpy as np
import os
import re

infofile='riverinfo.dat'
ncfile='river_grid_fluxes_daily.nc'
setupdir='/home/onur/setups/mossco-setups/sns/'
predefinedindices=False
predefined={'r001':[33,129],'r002':[12,78],'r003':[19,112],'r004':[29,46],'r005':[10,7],'r006':[22,31],'r007':[14,21],'r008':[13,17]}

ncfname=os.path.join(setupdir,ncfile)
riverinfofile = os.path.join(setupdir, infofile)

if predefinedindices:
    inds=predefined
else:
    inds=OrderedDict()
    fid = open(riverinfofile,'r')
    nriver=int(fid.readline())
    for line in fid:
        words=re.split(r' *',line)
        name=words[3].split('\n')[0]
        inds[name]=[int(words[2])-1,int(words[1])-1]


nc=netCDF4.Dataset(ncfname)

#time
tv=nc.variables['time']
utime=netcdftime.utime(tv.units)
tvec=utime.num2date(tv[:])

vV=nc.variables['volume_flux_in_water']
vN=nc.variables['Dissolved_Inorganic_Nitrogen_DIN_nutN_flux_in_water']
vP=nc.variables['Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water']
vV2=deepcopy(vV[:])
vV2.mask=np.ma.nomask
vN2=deepcopy(vN[:])
vN2.mask=np.ma.nomask
vP2=deepcopy(vP[:])
vP2.mask=np.ma.nomask

nc.close()

for rivno,river in enumerate(inds.keys()):
    ind=inds[river]
    print river+':'
    for ti,t in enumerate(tvec):
        print ' '+str(t.date())+' V:'+str(vV2[ti,ind[0],ind[1]])+' N:'+str(vN2[ti,ind[0],ind[1]])+' P:'+str(vP2[ti,ind[0],ind[1]])

if not predefinedindices:
    print '\nfor indices read from:'+infofile,
else:
    print '\nfor predefined indices:',
print inds

