#!/usr/bin/env python
#> @brief Script to create from riverinfo and river.nc flux input

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2015,2016 Helmholtz Zentrum Geesthacht
#> @author Carsten Lemmen <carsten.lemmen@hzg.de>
#> @author Onur Kerimoglu <onur.kerimoglu@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

import numpy as np
import netCDF4,sys
from netcdftime import utime
import os.path
import re
import datetime as dt
from scipy import interpolate
from pylab import *

MV=-1E30 #MISSING VALUE in the river_grid_fluxes.nc to be created
FV=-1E20 #FILL VALUE

fnamedict={
            # 'Daugava':'large_rivers_BalticSea_LAT_GR_Daugawa',
            # 'Neman':'large_rivers_BalticSea_LIT_BP_Njemen',
            # 'Odra':'large_rivers_BalticSea_PL_BP_Oder',
            # 'Vistula':'large_rivers_BalticSea_PL_BP_Weichsel', #no Weichsel in riverinfo.dat
            # 'Pregolia':'large_rivers_BalticSea_RUS_BP_Pregel',
            # 'Neva':'large_rivers_BalticSea_RUS_GF_Neva',
            # 'LuleAlv':'large_rivers_BalticSea_SW_BoB_Lulealv',
            # 'Angermanalven':'large_rivers_BalticSea_SW_BoS_Angermansalv',
            # 'UmeAlv':'large_rivers_BalticSea_SW_BoS_Umealv',
            # 'GotaAlv':'large_rivers_BalticSea_SW_Kat_Goeta',
            'r001':'Cefas_ELBE_1999-2016',
            'r002':'Cefas_EMS_1999-2016',
            'r003':'Cefas_WESER_1999-2016',
            'r004':'Cefas_LAKE_IJSSEL_1999-2016',
            'r005':'Cefas_SCHELDT_1999-2016',
            'r006':'Cefas_NORTH_SEA_CANAL_1999-2016',
            'r007':'Cefas_RHINE_1999-2016',
            'r008':'Cefas_MEUSE_1999-2016',
            'r009':'Cefas_HUMBER_1999-2016',
            'r010':'Cefas_WASH_1999-2016'
            }
Nfractions=  {'r001':1.0,
            'r002':1.0,
            'r003':1.0,
            'r004':1.0,
            'r005':1.0,
            'r006':1.0,
            'r007':1.0,
            'r008':1.0,
            'r009':1.0,
            'r010':1.0
            }
Pfractions={'r001':1.0,
            'r002':1.0,
            'r003':1.0,
            'r004':1.0,
            'r005':1.0,
            'r006':1.0,
            'r007':1.0,
            'r008':1.0,
            'r009':1.0,
            'r010':1.0
            }

#specify fraction of each class to be assumed for the input data 
SPMfraction={'r009':{'SPM_001': 0.375, 'SPM_002': 0.5, 'SPM_003': 0.125},
             'r010':{'SPM_001': 0.375, 'SPM_002': 0.5, 'SPM_003': 0.125},
            }

#specify constant concentrations in mg/l (entries for which the SPMfraction is specified and data exist, will be ignored)
SPMconstant={'r001':{'SPM_001': 14.25, 'SPM_002': 19.0, 'SPM_003': 4.75},
             'r002':{'SPM_001': 7.5,   'SPM_002': 10.0, 'SPM_003': 2.5},
             'r003':{'SPM_001': 13.125,'SPM_002': 17.5, 'SPM_003': 4.375},
             'r004':{'SPM_001': 9.0,   'SPM_002': 12.0, 'SPM_003': 3.0},
             'r005':{'SPM_001': 37.5,  'SPM_002': 50.0, 'SPM_003': 12.50},
             'r006':{'SPM_001': 11.25, 'SPM_002': 15.0, 'SPM_003': 3.75},
             'r007':{'SPM_001': 11.25, 'SPM_002': 15.0, 'SPM_003': 3.75},
             'r008':{'SPM_001': 11.25, 'SPM_002': 15.0, 'SPM_003': 3.75},
            }

riverDict={'nutN':'Dissolved_Inorganic_Nitrogen_DIN_nutN_flux_in_water',
           'nutP':'Dissolved_Inorganic_Phosphorus_DIP_nutP_flux_in_water',
           'detN': 'Detritus_Nitrogen_detN_flux_in_water',
           'detP':'Detritus_Phosphorus_detP_flux_in_water',
           'detC': 'Detritus_Carbon_detC_flux_in_water',
           'domN': 'Dissolved_Organic_Nitrogen_domN_flux_in_water',
           'domP': 'Dissolved_Organic_Phosphorus_domP_flux_in_water',
           'domC': 'Dissolved_Organic_Carbon_domC_flux_in_water',
           'phyN':'Phytplankton_Nitrogen_phyN_flux_in_water',
           'phyP':'Phytplankton_Phosphorus_phyP_flux_in_water',
           'phyC': 'Phytplankton_Carbon_phyC_flux_in_water',
           'Rub':'fraction_of_Rubisco_Rub_flux_in_water',
           'chl':'Chl_chl_flux_in_water'}

riverDict_SPM={'SPM_001':'SPM_001_flux_in_water',
               'SPM_002':'SPM_002_flux_in_water',
               'SPM_003':'SPM_003_flux_in_water'
              }

def read_topo_ncdf(ncfile):
  if not os.path.exists (ncfile):
    raise RuntimeError(ncfile+' does not exist')
  nc = netCDF4.Dataset(ncfile)
  ncv = nc.variables
  if ncv['grid_type'][0]==3.0:
    lon=range(1,len(nc.dimensions['x_T'])+1)
    lat=range(1,len(nc.dimensions['y_T'])+1)
  else:
    lon = np.squeeze(ncv['lon'][:])
    lat = np.squeeze(ncv['lat'][:])
  nc.close()
  return lon,lat

def generate_riverfile(setupdir,datadir,yearspan,dospm=False,doplot=True):

  if doplot:
    try:
      import matplotlib.pyplot as plt
      plotdir=os.path.join(setupdir,'Plot','Rivers')
      if not os.path.exists(plotdir):
        os.makedirs(plotdir)
    except:
      doplot=False
      print  'either matplotlib could not be imported, or you are not in an X-environment. Will skip the plots'


  unit_discharge='m3 s-1'
  if dospm:
     variables = riverDict_SPM.values()
     unit='g s-1'
  else:
     variables=riverDict.values()
     unit='mmol s-1'


  #constants
  topofile=os.path.join(setupdir, 'Topo','topo.nc')
  lon, lat  = read_topo_ncdf(topofile)

  riverinfofile = os.path.join(setupdir, 'riverinfo.dat')
  fid = open(riverinfofile,'r')
  nriver=int(fid.readline())
  ix={}
  iy={}
  names=[]

  mintime=dt.datetime.strptime(str(yearspan[0])+'-01-01 00:00:00','%Y-%m-%d %H:%M:%S')
  maxtime=dt.datetime.strptime(str(yearspan[1])+'-12-31 00:00:00','%Y-%m-%d %H:%M:%S')
  #maxtime=dt.datetime.strptime(str(yearspan[0])+'-02-28 00:00:00','%Y-%m-%d %H:%M:%S')
  print str(mintime)+' - '+str(maxtime)

  tdiff=maxtime-mintime
  days=np.array(range(0,tdiff.days,7*1))
  seconds=days * 86400.0 # in seconds

  i=0
  for line in fid:
    words=re.split(r' *',line)
    names.append( re.sub('\n','',words[3]))
    iy[names[i]]=int(words[2]) #indexes for latitudes
    ix[names[i]]=int(words[1]) #indexes for longitudes
    i=i+1

  fid.close()

  if dospm:
    filename = os.path.join(setupdir, 'river_grid_fluxes_spm.nc')
  else:
    filename = os.path.join(setupdir, 'river_grid_fluxes.nc')

  ncout=netCDF4.Dataset(filename,'w',format='NETCDF3_CLASSIC')
  ncout.setncattr('history','')
  nx=len(lon)
  ny=len(lat)

  ncout.createDimension('time',0)
  v = ncout.createVariable('time','f8',('time'))
  v.axis='T'
  v.units='seconds since ' + str(mintime)

  if np.min(lon)==0 and np.max(lon)==nx:
    lat_dim='x'
    lon_dim='y'
  else:
    lat_dim='lat'
    lon_dim='lon'

  ncout.createDimension(lon_dim,nx)
  ncout.createDimension(lat_dim,ny)

  v = ncout.createVariable(lat_dim,'f8',(lat_dim))

  if lat_dim=='lat':
    v.axis='Y'
    v.units='degree_north'
    v.valid_min=-90.0
    v.valid_max=90.0
  else:
    v.axis='Y'
    v.units=''
    v.valid_min=0
    v.valid_max= sys.maxint

  v = ncout.createVariable(lon_dim,'f8',(lon_dim))
  if lon_dim=='lon':
    v.axis='X'
    v.units='degree_east'
    v.valid_min=-180.0
    v.valid_max=180.0
  else:
    v.axis='X'
    v.units=''
    v.valid_min=0
    v.valid_max= sys.maxint

  v = ncout.createVariable('volume_flux_in_water','f8',('time',lat_dim,lon_dim),fill_value=FV)
  v.missing_value=MV
  v.units=unit_discharge
  v.valid_min=0.0
  v.valid_max=1E30

  #initialize volume fluxes to 0.0
  v[:]=zeros((len(seconds),len(lat),len(lon)))
  #ncout.variables['volume_flux_in_water'][:,:,:]=zeros((len(seconds),len(lat),len(lon)))

  ncout.variables['time'][:]=seconds
  ncout.variables[lat_dim][:]=lat
  ncout.variables[lon_dim][:]=lon

  # initialize ncout utime
  out_ut = utime(ncout.variables['time'].units)
  jd_out_date=out_ut.num2date(ncout.variables['time'][:])
  jd_out = date2num(jd_out_date)

  for i in range(0,len(variables)):
    v = ncout.createVariable(variables[i],'f8',('time',lat_dim,lon_dim),fill_value=FV)
    v.missing_value=MV
    v.valid_min=0.0
    v.valid_max=1E12
    v.units=unit

    #v[:,:,:]=0.0
    v[:]=zeros((len(seconds),len(lat),len(lon)))
    #ncout.variables[variables[i]][:,:,:]=1.0

  for i in range(0,nriver):
    if names[i] in fnamedict.keys():
        fname=fnamedict[names[i]]
    else:
        fname=names[i]
    riverfile=os.path.join(datadir, fname+'.nc')

    print names[i]+ ': ' + riverfile

    #time conversion
    timeconv = 1.0  # already in m3/s
    if 'BalticSea' in fname.split('_'): #
        timeconv = 1.0 / 86400  # m3/d to m3/s


    getmnc = netCDF4.Dataset(riverfile)
    gncv = getmnc.variables
    # create utime object and create julian days
    getm_ut = utime(gncv['time'].units)
    jd_getm = date2num(getm_ut.num2date(gncv['time'][:]))
    #discharge = gncv[names[i]][:]
    discharge = gncv['discharge'][:]*timeconv #final: [m3/s]

    #keep only the valid values (to make sure interpolation later works)
    valind=where(discharge!=gncv['discharge'].missing_value)
    discharge=discharge[valind]
    jd_getm=jd_getm[valind]

    if not os.path.isfile(riverfile):
      print riverfile + ': not found.'#riverfile
      matter_present=False
    else:
      matter_present=True

      # create utime object and create julian days
      in_ut = utime(gncv['time'].units)
      jd_in_date=in_ut.num2date(gncv['time'][:])
      jd_in = date2num(jd_in_date)


    if doplot and matter_present:
        fig = plt.figure(figsize=(6,6), dpi=96)
        fig.subplots_adjust(top=.9,hspace=1.3, wspace=0.2)
        plt.figtext(0.9, 0.97, names[i], ha='right', va='top', size='large')

    f=interpolate.interp1d(jd_getm,discharge,bounds_error=False, fill_value=ncout.variables['volume_flux_in_water'].missing_value)
    y1=f(jd_out)
    wrongind=y1<0.0
    #y1[wrongind]=ncout.variables['volume_flux_in_water'].missing_value
    y1[wrongind]=0.0
    if sum(wrongind)>0:
        print 'WARNING: %s values were replaced with fill_value=%s'%(sum(wrongind),ncout.variables[value].missing_value )
    ncout.variables['volume_flux_in_water'][:,iy[names[i]]-1,ix[names[i]]-1]=y1
    ncout.variables['volume_flux_in_water'].multiplication_factor = gncv['discharge'].multiplication_factor

    #print str(ix[names[i]]-1)+','+str(iy[names[i]]-1)+': Wrote values for Discharge in volume_flux_in_water'

    if matter_present:
      varno=0
      if dospm:
         # check if any fraction is specified for the current river
         data_exist=False
         if names[i] in SPMfraction:
             SPMdata=gncv['SPM'][:]
             if sum(SPMdata)>0:
                 data_exist=True
                 fract_R=SPMfraction[names[i]]

         #if data does not exist, check if a constant concentration is specified
         if (not data_exist) and (names[i] in SPMconstant):
             const_R=SPMconstant[names[i]]
         else: #otherwise assume 0 concentration for all classes
             const_R={}
             for key in riverDict_SPM.keys():
                 const_R[key]=0.0

         for key, value in riverDict_SPM.iteritems():

            if data_exist:
                concentration=SPMdata*fract_R[key]
            else:
                concentration=const_R[key]

            # discharge coming with the nutrient concentrations is used here to calculate the mass flux per time
            y = concentration * gncv['discharge'][:] * timeconv
            # [g/s]= [g m-3]  * [          m3/s               ](units assumed for concentrations and flow rate)

            # interpolate to the specified time resolution, within the specified time span:
            y1= f = interpolate.interp1d(jd_in[valind], y[valind], bounds_error=False, fill_value=0.0)
            y1 = f(jd_out)

            ncout.variables[value][:, iy[names[i]] - 1, ix[names[i]] - 1] = y1
            #print str(ix[names[i]] - 1) + ',' + str(iy[names[i]] - 1) + ': Wrote values for ' + key + ' in ' + value
      else:
         for key,value in riverDict.iteritems():
            varno += 1
            if not gncv.has_key(key): continue

            oldfract=gncv[key].multiplication_factor;
            newfract=1.0
            if key=='DIN':
             if Nfractions.has_key(names[i]):
                newfract=Nfractions[names[i]]
                fract_warning='DIN(*%s)*%s'%(str(oldfract),str(newfract))
            elif key=='PO4':
             if Pfractions.has_key(names[i]):
                newfract=Pfractions[names[i]]
                fract_warning = 'DIP(*%s)*%s' % (str(oldfract), str(newfract))
            if newfract*oldfract!=1.0:
             ncout.setncattr('history', ncout.getncattr('history')+'\n'+value+'('+names[i]+') x'+str(newfract*oldfract))

            # discharge coming with the nutrient concentrations is used here to calculate the mass flux per time
            y=gncv[key][:] * gncv['discharge'][:] *timeconv *newfract
            # [mmol/s]= [mmol/m3] * [m3/d1]  * [d/s] (units assumed for concentrations and flow rate)

            # interpolate to the specified time resolution, within the specified time span:
            # but interpolate only with the valid values
            invalind = where(np.isnan(y) == True)  # check for NaN and make them invalid
            y[invalind] = gncv[key].missing_value
            valind = where(y != gncv[key].missing_value)
            f=interpolate.interp1d(jd_in[valind],y[valind],bounds_error=False,fill_value=0.0)
            y1=f(jd_out)
            wrongind=y1<0.0
            #y1[wrongind]=ncout.variables[value].missing_value
            y1[wrongind]=0.0
            if sum(wrongind)>0:
                print 'WARNING: %s values were replaced with fill_value=%s'%(sum(wrongind),ncout.variables[value].missing_value )
            print '. '

            ncout.variables[value][:, iy[names[i]] - 1, ix[names[i]] - 1] = y1
            print str(ix[names[i]] - 1) + ',' + str(iy[names[i]] - 1) + ': Wrote values for ' + key + ' in ' + value

      if doplot and matter_present:

        plt.subplot(len(riverDict),1, varno)
        plt.plot(jd_in_date,y,'-',color='gray', linewidth=0.5)
        plt.plot(jd_out_date,y1,'-',color='k', linewidth=2.5)
        plt.title(key+ '[mmol d-1]')

        plotfname=os.path.join(plotdir,names[i]+'.png')
        plt.savefig(plotfname, dpi=150)
        print 'figure saved:'+plotfname
        plt.close(fig)

      getmnc.close()

  ncout.close()
  print 'created: '+ filename

if __name__ == '__main__':
    if len(sys.argv)>1:
        setupdir=sys.argv[1]
    else:
        setupdir='/home/wirtz/mossco/setups/sns'
        #setupdir='/Users/lemmen/devel/MOSSCO/setups/NSBS6nm'
   #     setupdir='/net/themis/meer/nordsee/MOSSCO/03_Modelle/02_MOSSCO/mossco-setups/sns'
        if not os.path.exists(setupdir):
            raise RuntimeError('default setup-directory could not be found')
        #fname='soil_mossco_gffn_stitched.nc'

    if len(sys.argv)>2:
        datadir=sys.argv[2]
    else:
        #datadir='/home/onur/WORK/projects/GB/data/Rivers/maecs_sns/Cefas_rivers_1999-2016_GapsChecked_getmU_hzg_maecs_obsDIM-OM_fixedMV_r12_N085P095C095/'
        datadir='/home/wirtz/Downloads/'
#        datadir='/net/themis/meer/nordsee/MOSSCO/03_Modelle/02_MOSSCO/mossco-setups/sns/Forcing/River/'
        if not os.path.exists(datadir):
            raise RuntimeError('default data-directory could not be found')
        #fname='soil_mossco_gffn_stitched.nc'

    if len(sys.argv)>3:
        yearspan=sys.argv[3].split(',')
    else:
        yearspan=[1999,2016]
        #yearspan = [2009, 2016]


    generate_riverfile(setupdir,datadir,yearspan,dospm=True,doplot=True)
