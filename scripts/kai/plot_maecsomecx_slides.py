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
lonb=[3.0, 10.]
latb=[51., 55.5]

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
if len(sys.argv)>1:
    ncfile=sys.argv[1]
else:
    ncfile='cut_stitched.nc'

if len(sys.argv)>2:
    varns=sys.argv[2].split(',')
else:
#    varns=['Chl_chl_in_water']
    varns=['Chl_chl_in_water',
#    'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water',
    'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water',
#    'P:C_ratio__QP_in_water',
#    'chlorophyll_to_carbon_ratio_in_water',
#    'denitrification_rate_in_soil',
#    'mole_concentration_of_nitrate_in_soil',
    'mole_concentration_of_phosphate_in_soil']


varDict={'Chl_chl_in_water':{'unit':'mg m$^{-3}$','name':'Chlorophyll','minmax':[0.05,30]},
'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water':{'unit':'mmol m$^{-3}$','name':'Pelagic DIN','minmax':[4,250]},
'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water':{'unit':'mmol m$^{-3}$','name':'Pelagic DIP','minmax':[0.05,2.]},
'P:C_ratio__QP_in_water':{'unit':' ','name':'P:C ratio','minmax':[0.0,0.02]},
'chlorophyll_to_carbon_ratio_in_water':{'unit':' ','name':'Chl:C ratio','minmax':[0.0,0.1]},
'denitrification_rate_in_soil':{'unit':'mmol m$^{-3}$ d$^{-1}$','name':'Denitrification','minmax':[0.08,125]},
'mole_concentration_of_nitrate_in_soil':{'unit':'mmol m$^{-3}$','name':'Soil NO$_3$','minmax':[5,40]},
'temperature_in_water':{'unit':'^oC','name':'Water Temperature','minmax':[13.0,18.5]},
'mole_concentration_of_phosphate_in_soil':{'unit':'mmol m$^{-3}$','name':'Soil PO$_4$','minmax':[0.02,50]}}

# open data file and create variables object
print(ncfile)
nc=netCDF4.Dataset(ncfile)
ncv=nc.variables

# time-related issues
# read time as python datetime object
tv = ncv['time']
utime=netcdftime.utime(tv.units)
time = utime.num2date(tv[:])
tnum=len(time)

if len(sys.argv)>4:
    asplit=sys.argv[4].split('-')
    if len(asplit)>1:
        tmin=int(asplit[0])
        tmax=int(asplit[1])
    else:
        tmin=int(sys.argv[4])
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

      var=squeeze(ncv[varn][t,1,:,:]) #[t,-1,:,:]
      cbtitle=unit
      os.system('mkdir -p %s'%varn)

      f=figure(figsize=(5.5,6),dpi=dpi)
      f.subplots_adjust(left=0.0,right=1.0,bottom=0.0,top=1.0)

      if (name.find('ratio')>-1):
        pcf=proj.pcolormesh(xx,yx,var,cmap=cm.terrain)
      else:
        pcf=proj.pcolormesh(xx,yx,var,cmap=cm.terrain,norm = LogNorm())

      if len(minmax)==2:
         clim(minmax[0],minmax[1])

      proj.drawcoastlines()
      proj.fillcontinents((0.8,0.9,0.8))

      xt,yt=proj(5.3,51.5)
      text(xt,yt,name,size=22.,color='k')
      xt,yt=proj(5.3,51.3)
      text(xt,yt,(str(time[t]))[0:16],size=12.,color='k')

      timestr=re.sub('-','',str(time[t]))[0:8]

      cax=axes([0.78,0.06,0.05,0.37])
      colorbar(pcf,cax=cax)
      cax.set_title(cbtitle,size=12.)

      savefig('%s/%s_%s.png'%(varn,varn,timestr),dpi=dpi)
      close()