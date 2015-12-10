#!/usr/bin/env python
#> @brief Adds MAECS-specific fluxes to river forcing file

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2015 Helmholtz Zentrum Geesthacht
#> @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

import netCDF4 as netcdf
import sys

if len(sys.argv) > 1:
  infile = sys.argv[1]
else:
  infile = u"river_grid_fluxes.nc"

if len(sys.argv) > 2:
  outfile = sys.argv[2]
else:
  outfile =  u"river_grid_fluxes_nuts.nc"

nc    = netcdf.Dataset(infile,'r')
ncout = netcdf.Dataset(outfile, 'w', format='NETCDF4_CLASSIC')

# Copy all dimensions
for key,value in nc.dimensions.iteritems():
  if ncout.dimensions.has_key(key): continue
  else:
    ncout.createDimension(key,len(nc.dimensions[key]))

# Copy all variables
for key,value in nc.variables.iteritems():
  dims=list(value.dimensions)

  try:
    var=ncout.createVariable(key,refvalue.dtype,tuple(dims),fill_value=value.getncattr('_FillValue'))
  except:
    var=ncout.createVariable(key,value.dtype,tuple(dims))

  for att in value.ncattrs():
    if att == '_FillValue': continue
    else:
      var.setncattr(att,value.getncattr(att))

  ncout.variables[key][:]=value[:]

  print 'Created for output variable ', key , tuple(dims)

refdims=nc.variables['volume_flux_in_water'].dimensions
refvalue=nc.variables['volume_flux_in_water']


# List of new variables (could be an argv later)
additional_items={
  'fraction_of_Rubisco_Rub_flux_in_water':0.2,
  'Zooplankton_Carbon_zooC_flux_in_water':0.02,
  'Phytplankton_Carbon_phyC_flux_in_water':1.0,
  'Phytplankton_Phosphorus_phyP_flux_in_water':1.0,
  'Phytplankton_Nitrogen_phyN_flux_in_water':1.0,
  'Chl_chl_in_water':0.2
}
# todo: add phyS and frac_chl (but find out the MOSSCO names first)
#phyS = 0.1,
#frac_chl = 0.2,

# Create new variables if not present
for key,value in additional_items.iteritems():
  if ncout.variables.has_key(key): continue

  try:
    var=ncout.createVariable(key,refvalue.dtype,tuple(refdims),fill_value=refvalue.getncattr('_FillValue'))
  except:
    var=ncout.createVariable(key,refvalue.dtype,tuple(refdims))

  print 'Created for output variable ', key , tuple(refdims)

for key,value in additional_items.iteritems():
    ncout.variables[key][:]=refvalue[:]*0.0+value
    print key, value
nc.close()

ncout.sync()
ncout.close()
