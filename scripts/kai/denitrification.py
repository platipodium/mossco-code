from pylab import *
import pickle
from numpy import load as nload
from mpl_toolkits.basemap import Basemap
import netCDF4

def plot_station(proj,lon,lat,name,unsure=False,right=False):
  xx,yy = proj(lon,lat)
  if unsure:
      facecolor=(0.9,0.8,0.8)
      edgecolor=(0.3,0.3,0.3)
  else:
      facecolor='orange'
      edgecolor='red'
  color='k'
  proj.plot([xx,],[yy,],'o',markersize=10, \
            markeredgecolor=edgecolor,markerfacecolor=facecolor, \
            markeredgewidth=3.)
  if right:
    xoffset,yoffset=(25000,10000) # m
  
    text(xx+xoffset,yy-yoffset,name,horizontalalignment='left', \
            verticalalignment='top',size=10,color=color)
  else:
    xoffset,yoffset=(25000,10000) # m
  
    text(xx-xoffset,yy+yoffset,name,horizontalalignment='right', \
            verticalalignment='top',size=10,color=color)

doproj=True

if doproj:
  # initialise geographic projection
  proj=Basemap(projection='lcc',
               resolution='i',
               llcrnrlon=-1.0,
               llcrnrlat=50.5,
               urcrnrlon=26.0,
               urcrnrlat=59.5,
               lat_0=54.0,
               lon_0=20.)
# RH: better: 
#  proj=Basemap(projection='lcc',
#               resolution='i',
#               llcrnrlon=-1.0,
#               llcrnrlat=51.0,
#               urcrnrlon=26.0,
#               urcrnrlat=59.5,
#               lat_0=54.0,
#               lon_0=10.)

  fh=open('proj.pickle','wb')
  pickle.dump((proj,),fh,protocol=-1)
  fh.close()
else:
  (proj,) = nload('proj.pickle')

# read bathymetry
#nc=netCDF4.Dataset('../Topo/NSBS6nm.v01.nc')
nc=netCDF4.Dataset('npzd_soil_stitched.nc')
#nc=netCDF4.Dataset('maecsomexdia_soil_stitched.nc')

ncv=nc.variables

h = ncv['denitrification_rate_in_soil'][:]
#h = ncv['bathymetry'][:]
#lon = ncv['lon'][:]
#lat = ncv['lat'][:]

lon = ncv['lon_2'][:]
lat = ncv['lat_2'][:]
nc.close()
#h[where(h > 250.)]=250.

LO,LA = meshgrid(lon,lat)
x,y = proj(LO,LA)

# map

for i in range(9,h.shape[0]):
  f = figure(figsize=(10,6), dpi=96)
  f.subplots_adjust(left=0.0,right=1.0,bottom=0.0,top=1.0)

#proj.contourf(x,y,h,levels=range(0,255,25),extend='both',cmap=cm.YlGnBu)
  proj.contourf(x,y,h[i,:,:],extend='both',levels=np.arange(0.0,1.0,0.1),cmap=cm.YlGnBu)
  proj.pcolor(x,y,h[i,:,:],cmap=cm.YlGnBu)

#proj.drawcoastlines(color=(0.7,0.7,0.7),linewidth=0.2)
#proj.fillcontinents((0.7,0.7,0.7),lake_color=(0.9,0.9,1.0))
  proj.drawcoastlines(color=(0.3,0.3,0.3),linewidth=0.5)
  proj.fillcontinents((1.0,1.0,1.0),lake_color=(0.9,0.9,1.0))
  gca().patch.set_facecolor((0.9,0.9,1.0))

  deutsch=False
  if deutsch:
    plot_station(proj,7.892,54.189,'Helgoland Reede\n25 m')
    plot_station(proj,8.073,54.061,'NOAH Station C',right=True)
    plot_station(proj,8.4412,55.017,'List Koenigshafen\n10 m')
    plot_station(proj,20.05,57.33,'Gotland Station 271\n240 m')
    plot_station(proj,12.685,54.411,'Zingster Bodden\n5 m',unsure=True)

    xx,yy=proj(1.0,56.5)
    text(xx,yy,'Nordsee',size=18.,color=(0.7,0.7,0.7))

    xx,yy=proj(16.,56.5)
    text(xx,yy,'Ostsee',size=18.,color=(0.7,0.7,0.7),rotation=30.)
    filename='Referenzstationen_Karte.pdf'
  else:
    #plot_station(proj,7.892,54.189,'Helgoland\n25 m')
    #plot_station(proj,8.4412,55.017,'List Koenigshafen\n10 m')
    #plot_station(proj,20.05,57.33,'Gotland Station 271\n240 m')
    #plot_station(proj,12.685,54.411,'Zingst Bay\n5 m',unsure=True)

    xx,yy=proj(1.0,56.5)
    text(xx,yy,'North Sea',size=18.,color=(0.3,0.3,0.3))

    xx,yy=proj(16.,56.5)
    text(xx,yy,'Baltic Sea',size=18.,color=(0.3,0.3,0.3),rotation=30.)

  filename='denitrification_' + str(i) + '.pdf'
  savefig(filename,dpi=300)
  s=show()
  close()

