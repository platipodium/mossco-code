from pylab import *
from mpl_toolkits.basemap import Basemap
import os,sys
import numpy as np
import pickle
import netCDF4
import netcdftime
import re
from matplotlib.colors import LogNorm

# set map boundaries
lonb=[2., 9.5]
latb=[51.2, 55.5]

# try to load projection object, if not present
# re-initiate projection object
# and save as proj.pickle for later usage
if os.path.isfile('proj.pickle'):
    (proj,)=np.load('proj.pickle')
else:
    proj=Basemap(projection="merc", lat_ts=0.5*(latb[1]+latb[0]), resolution="h",llcrnrlon=lonb[0], urcrnrlon=lonb[1], llcrnrlat=latb[0], urcrnrlat=latb[1])
    f=open('proj.pickle','wb')
    pickle.dump((proj,),f)
    f.close()

# get commandline options:
#  python plot_surface.py ncfile var1,var2 minval,maxval timestep_start-timstep_end
if len(sys.argv)>3:
    ncfile=sys.argv[3]
else:
#    ncfile='cut_stitched.nc'
    ncfile='sns_pQ10_z.nc'
varn0=-1
if len(sys.argv)>2:
    varns=sys.argv[2].split(',')
else:
#    varns=['Chl_chl_in_water']
    varns0=['Chl_chl_in_water',
    'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water',
    'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water',
    'PC','NC','Detritus_Carbon_detC_in_water',
#    'chlorophyll_to_carbon_ratio_in_water',
    'denitrification_rate_in_soil',
    'mole_concentration_of_nitrate_in_soil',
    'dissolved_oxygen_upward_flux_at_soil_surface']
#    'mole_concentration_of_phosphate_in_soil'
    varns=[
#    'denitrification_rate_in_soil','detritus-P_in_soil',
#    'mole_concentration_of_nitrate_in_soil','mole_concentration_of_phosphate_in_soil',
    'dissolved_oxygen_in_soil','dissolved_reduced_substances_upward_flux_at_soil_surface',
    'dissolved_oxygen_upward_flux_at_soil_surface',  
    'fast_detritus_C_in_soil','N2flux']

varDict={'Chl_chl_in_water':{'unit':'mg m$^{-3}$','name':'Chlorophyll','minmax':[0.1,20]},
'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water':{'unit':'mmol m$^{-3}$','name':'Pelagic DIN','minmax':[0,50]},
'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water':{'unit':'mmol m$^{-3}$','name':'Pelagic DIP','minmax':[0.0,1.3]},
'PC':{'unit':' ','name':'P:C ratio','minmax':[0.004,0.016]},'NC':{'unit':' ','name':'N:C ratio','minmax':[0.05,0.3]},
'dissolved_oxygen_upward_flux_at_soil_surface':{'unit':'mmol m$^{-2}$ d$^{-1}$','name':'O$_2$ flux','minmax':[0,24]},
'dissolved_reduced_substances_upward_flux_at_soil_surface':{'unit':'mmol m$^{-2}$ d$^{-1}$','name':'ODU flux','minmax':[0,14]},'Detritus_Carbon_detC_in_water':{'unit':'mmol-C m$^{-3}$','name':'Pel. Detritus','minmax':[0,16]},
'chlorophyll_to_carbon_ratio_in_water':{'unit':' ','name':'Chl:C ratio','minmax':[0.0,0.1]},
'denitrification_rate_in_soil':{'unit':'mmol-N m$^{-3}$ d$^{-1}$','name':'Denitrification','minmax':[0.08,50]},
'mole_concentration_of_nitrate_in_soil':{'unit':'mmol m$^{-3}$','name':'Soil NO$_3$','minmax':[1,50]},
'dissolved_oxygen_in_soil':{'unit':'mmol m$^{-3}$','name':'Soil O$_2$','minmax':[150,300]},
'fast_detritus_C_in_soil':{'unit':'mmol-C m$^{-3}$','name':'Soil Detritus','minmax':[20,2000]},
'N2flux':{'unit':'$\mu$mol-N m$^{-2}$ h$^{-1}$','name':'Tot.denitrif.','minmax':[0,80]},
'mole_concentration_of_phosphate_in_soil':{'unit':'mmol m$^{-3}$','name':'Soil PO$_4$','minmax':[0.5,8]},
'fast_detritus_C_in_soil':{'unit':'mmol m$^{-3}$','name':'TOC fresh (soil)','minmax':[0.,30000]}
}

# open data file and create variables object
#print(ncfile)
nc=netCDF4.Dataset(ncfile)
ncv=nc.variables

# time-related issues
# read time as python datetime object
tv = ncv['time']
utime=netcdftime.utime(tv.units)
time = utime.num2date(tv[:])
tnum=len(time)

if len(sys.argv)>1:
    asplit=sys.argv[1].split('-')
    if len(asplit)>1:
        tmin=int(asplit[0])
        tmax=int(asplit[1])
    else:
        tmin=int(sys.argv[1])
        tmax=tmin+1
else:
    tmin=0
    tmax=tnum

#tmax=1

# get coordinate names
coords = ncv[varns[0]].coordinates.split(' ')
#lonname = coords[0]
#latname = coords[1]
lonname = coords[-1]
latname = coords[-2]

# read coodinates in the range
#  specific to the gb_curv setup
lons=ncv[lonname][:,:]
lats=ncv[latname][:,:]

# central plotting switches (CHANGE_HERE):
dpi=96
xx,yx = proj(lons,lats)

for t in range(tmin,tmax):

    for varn in varns:

      if varDict.has_key(varn):
        unit=varDict[varn]['unit']
        name=varDict[varn]['name']
        minmax=varDict[varn]['minmax']
      else:
        unit=ncv[varn].units
        name=varn
        minmax=[0.1,100]

      iz=1
      if (name.find('flux')>-1 or name.find('Tot.denitrif')>-1 ):
        var=squeeze(ncv[varn][t,:,:])
      else:
        if (name.find('in_soil')>-1 or name.find('Detritus') ):
          iz=0
        var=squeeze(ncv[varn][t,iz,:,:])

      if (name.find('ODU flux')>-1 ):
         varn0 = varn
         var=var*24*3600
      if (name.find('O$_2$ flux')>-1 and  varn0>-1):     
         var=(squeeze(ncv[varn0][t,:,:])-var)*24*3600
      if (name.find('Tot.denitrif')>-1):
        var=var*1000*0.8/24  #mmol/m3.d *m(por) -> \mumol m2.h (bulk)
#      if (name.find('flux')>-1 ):

      cbtitle=unit
      os.system('mkdir -p soil_%s'%varn)

      f=figure(figsize=(5.5,6),dpi=dpi)
      f.subplots_adjust(left=0.0,right=1.0,bottom=0.0,top=1.0)

      if (name.find('Chl')>-1 or name.find('Denitrification')>-1 or name.find('Soil NO$_3$')>-1 or name.find('Detritus')>-1):
        var[where(var<minmax[0])]=minmax[0]
        print minmax[0], name
        pcf=proj.pcolormesh(xx,yx,var,cmap=cm.gist_ncar,norm = LogNorm()) # cm.terrain
      else:
        pcf=proj.pcolormesh(xx,yx,var,cmap=cm.gist_ncar)#jet

      if len(minmax)==2:
        clim(minmax[0],minmax[1])

      proj.drawcoastlines()
      proj.fillcontinents((0.8,0.9,0.8))

      xt,yt=proj(5.05,51.8)
      text(xt,yt,name,size=22.,color='k')
      xt,yt=proj(5.1,51.3)
      text(xt,yt,(str(time[t]))[0:7],size=22.,color='k')

      timestr=re.sub('-','',str(time[t]))[0:8]

      nl=2;
      cyl=minmax[0] + np.arange(nl+1) * (minmax[1]-minmax[0])/nl
      cax=axes([0.79,0.04,0.05,0.37])
      cb=colorbar(pcf,cax=cax,ticks=cyl)
      cax.set_title(cbtitle,size=12.)
 #     cax.set_ticks(cyl) #, update_ticks=True
 #     cb.ax.yaxis.set_major_locator(matplotlib.ticker.MaxNLocator(nbins=8))

      savefig('soil_%s/%s_%d_%s.png'%(varn,varn,iz,timestr),dpi=dpi)
      close()
