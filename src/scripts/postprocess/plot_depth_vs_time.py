import netCDF4
import matplotlib.pyplot as plt
from matplotlib.pylab import cm
from matplotlib.colors import LogNorm
#from matplotlib.colors import Colormap as cm
import numpy as np
import sys
import matplotlib.mlab as mlab
import matplotlib

# get directory with files
if len(sys.argv)>1:
  filename=sys.argv[1]
else:
  #filename='nsbs-m-mossco_gffrpn.031.nc'
  filename='/Volumes/Kiwi/output/ns-m-gfsfrpn/mossco_gfsfrpn.084.nc'
  #filename='mossco_gfsfrpn_chl.031.nc'

if len(sys.argv)>2:
  varnames=sys.argv[2]
else:
  #varnames=[u'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water']
  #  varnames=[u'Chl_chl_in_water']
  varnames=['depth_averaged_x_velocity_in_water', 'depth_averaged_y_velocity_in_water'
            'dissolved_oxygen_upward_flux_at_soil_surface', 
            'dissolved_reduced_substances_upward_flux_at_soil_surface',
            'fast_detritus_C_upward_flux_at_soil_surface',
            'Chl_chl_in_water', 'Detritus_Carbon_detC_in_water',
            'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water',
            'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water',
            'Phytplankton_Carbon_phyC_in_water', 'Zooplankton_Carbon_zooC_in_water',
            'temperature_in_water', 'denitrification_rate_in_soil',
            'detritus-P_in_soil','dissolved_oxygen_in_soil',
            'dissolved_reduced_substances_in_soil','fast_detritus_C_in_soil',
            'mole_concentration_of_ammonium_in_soil','mole_concentration_of_nitrate_in_soil',
            'mole_concentration_of_phosphate_in_soil','slow_detritus_C_in_soil'
   ]

if len(sys.argv)>3:
  ix=sys.argv[3]
else:
  ix=0

if len(sys.argv)>3:
  iy=sys.argv[3]
else:
  iy=5

# open netcdf files

nc  = netCDF4.Dataset(filename)
ncv = nc.variables

# get time data
days=ncv['time'][:]/3600./24.
  
# set plot boundaries in days
x0=1
x1=365

data3={}
data2={}
units={}
for key,value in ncv.iteritems():
  if key in ['time', 'doy', 'date_string']: continue
  if not 'time' in value.dimensions: continue
  if (key.find('soil')<0 and key.find('water')<0): continue
    
  #print key, value.dimensions
  if 'getmGrid3D_getm_3' in value.dimensions or 'ungridded00024' in value.dimensions:
    # This is a 3D variable, create a curtain plot
    data3[key]=np.squeeze(value[:,:,ix,iy])
  elif  'getmGrid2D_getm_2'  in value.dimensions:
    # This is a 2D variable, create a line plot
    data2[key]=np.squeeze(value[:,ix,iy])
  
  try:  
    units[key]=value.units
  except:
    units[key]=''

if ncv.has_key('getmGrid3D_getm_layer'):
  water_depth=ncv['getmGrid3D_getm_layer'][:,ix,iy]
else:
  water_depth=[]

if ncv.has_key('layer_center_depth_in_soil'):
  soil_depth=ncv['layer_center_depth_in_soil'][:,ix,iy]
else:
  soil_depth=[]


# close netcdf files
nc.close()

 
n3=len(data3)
n2=len(data2)

i=0
ni=0
npx=np.min([2,len(varnames)])
npy=4#5

for key in sorted(data3):
  
  if len(varnames)>0 and not key in varnames: continue
  value=data3[key]
  
  if (value==0).all(): continue
  if len(np.unique(value))==1: continue

  if np.mod(i,npy*npx)==0:
    fig=plt.figure(figsize=(6,10))
    plt.subplots_adjust(right=0.8, wspace=0.5, hspace=0.3)

    i=0

  ax=plt.subplot(npy,npx,i+1)
  nz=np.shape(value)[1]
  print key, nz
  if key.find('water')>-1:
    if len(water_depth) == nz:
      z=-water_depth
      ylabel='depth (m)'
      plt.ylim(np.max(z), np.min(z))
    else:
      z=np.arange(0,nz,1)
      ylabel='level'  
      plt.ylim(np.min(z), np.max(z))
  elif key.find('soil')>-1:
    if len(soil_depth) == nz:
      z=soil_depth
      ylabel='depth (m)'
      plt.ylim(np.max(z), np.min(z))
    else:
      z=np.arange(0,nz,1)
      ylabel='level'  
      plt.ylim(np.max(z), np.min(z))

  
  ixmin=20
  ixmax=np.max(mlab.find(np.isfinite(value[:,0])))
  vmax=np.max(value[np.isfinite(value[ixmin:ixmax,0])])
  vmin=np.min(value[np.isfinite(value[ixmin:ixmax,0])])
  #(vmin,vmax)=(0,50)
  #if key.endswith('in_water'):
  #else:
 
  #cnt=plt.contourf(days,z,value.T,15,cmap=cm.YlGnBu,extend='neither',norm=matplotlib.colors.Normalize(vmin=vmin,vmax=vmax))
  cnt=plt.contourf(days,z,value.T,15,extend='neither',norm=matplotlib.colors.Normalize(vmin=vmin,vmax=vmax))
  #cnt=plt.contourf(days,z,value.T,15,norm=LogNorm(vmin=vmin,vmax=vmax))#, \
  #  norm=matplotlib.colors.Normalize(vmin=vmin,vmax=vmax))
  plt.title(key + ' ' + units[key],fontsize=np.sqrt(1.0/npx)*11)
  
  #if np.mod(i,npx)==0: 
  plt.ylabel(ylabel)
  #else:
  #  plt.gca().set_yticklabels([])
      
  plt.grid('on',axis='x')
   
  plt.gca().set_xticks([80,171,263,352])
  xt=plt.gca().get_xticks()  
  xtl=plt.gca().get_xticklabels()  
  #plt.xlim(days[ixmin],days[ixmax-1])
  
  pos = ax.get_position().get_points()
  cax=plt.axes([pos[1][0]*1.05, pos[0][1], (pos[1][0]-pos[0][0])/10.0, pos[1][1]-pos[0][1]])
  cbar=plt.colorbar(cnt,cax=cax,orientation='vertical', ticks=np.linspace(vmin,vmax,4))
  cbar.solids.set_edgecolor("face") # circumvents bug in svg abs

  decimals=np.floor(np.log10(np.max(np.abs(value[np.isfinite(value)]))))
  yt=np.linspace(vmin,vmax,4)
  yt=np.round(yt,int(2-decimals))
  cbar.set_ticks(yt)
  cyt=cax.get_yticks()
  cytl=cax.get_yticklabels()
  decimals=np.floor(np.log10(np.max(np.abs(value[np.isfinite(value)])))) 
  
  #cax.set_ylim(cyticks.min(),cyticks.max())

  if np.mod(i,npx*npy)==npx*npy-1:

    plt.savefig(filename+'_' + str(ni) + '.pdf')
    plt.close(fig)
    ni=ni+1
    
  i=i+1
  #exit

for key in sorted(data2):

  if len(varnames)>0 and not key in varnames: continue

  value=data2[key]
  if (value==0).all(): continue
  if len(np.unique(value))==1: continue

  if np.mod(i,npx*npy)==0:
    fig=plt.figure(figsize=(6,10))
    i=0
    
  plt.subplot(npy,npx,i+1)

  plt.plot(days,value,'b-',lw=2.0)
  plt.xlim(x0,x1)
  plt.title(key)
  plt.gca().set_xticklabels([])
  plt.grid('on',axis='x')

  if np.mod(i,npx*npy)==npx*npy-1:
    plt.savefig(filename+'_' + str(ni) + '.pdf')
    plt.savefig(filename+'_' + str(ni) + '.png')
    plt.close(fig)
    ni=ni+1

  i=i+1

plt.savefig(filename+'_' + str(ni) + '.pdf')
plt.savefig(filename+'_' + str(ni) + '.png')
plt.close(fig)

plt.close('all')
