from pylab import *
import pickle
from numpy import load as nload
from mpl_toolkits.basemap import Basemap
import netCDF4
import datetime
import netcdftime

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
#nc=netCDF4.Dataset('soil_mossco_gffn_stitched.nc')
nc=netCDF4.Dataset('netcdf_out_stitched.nc')

ncv=nc.variables
#varn = 'denitrification_rate_in_soil'
#varn = 'Detritus_Carbon_detC_in_water'
varn = 'Chl_chl_in_water'

var = ncv[varn][:]
unitstr=ncv[varn].units
tv = nc.variables['time'] # this is the variable (including attributes), data in eg., seconds
utime=netcdftime.utime(tv.units) #this is some intermediate variable
tvec=utime.num2date(tv[:]) #this is now human readable
print str(tvec[-1].date()) #prints the last date
#print str(tvec[-1].year()) #prints the last year, and etc.

bat = ncv['water_depth_at_soil_surface'][:]

#h = ncv['bathymetry'][:]
#lon = ncv['lon'][:]
#lat = ncv['lat'][:]

lon = ncv['getmGrid3D_getm_lon'][:]
lat = ncv['getmGrid3D_getm_lat'][:]
nc.close()
#h[where(h > 250.)]=250.

LO,LA = meshgrid(lon,lat)
x,y = proj(LO,LA)
z=2

# map
i0=var.shape[0]-5
for i in range(i0,i0+1):  #var.shape[0],10
  print 'plotting scene#:'+ str(i) + ' : ' + str(tvec[i].date()) 
  #+ datetime.datetime(2009, 1, 1, 1, 1, int(time[i]))
  #time.strftime('%Y-%m-%d', time.localtime(time[i]))
  f = figure(figsize=(10,6), dpi=96)
  f.subplots_adjust(left=0.0,right=1.0,bottom=0.0,top=1.0)
  
  h = 2*var[i,z,:,:] + 80./(5.0+bat[i,:,:])
  v = np.linspace(0, 20.0, 2, endpoint=True)
#proj.contourf(x,y,h,levels=range(0,255,25),extend='both',cmap=cm.YlGnBu),extend='both',levels=np.arange(0.0,1.0,0.1)
  proj.contourf(x,y,h,levels=np.arange(0.0,1.0,0.1),cmap=cm.jet ) #cm.YlGnBu
  pcf=proj.pcolor(x,y,h,cmap=cm.jet, vmin=0, vmax=10) #cm.YlGnBu YlOrRdnipy_spectral
  
  xx,yy=proj(3.0,59.)
  text(xx,yy,varn,size=24.,color=(0,0,0))

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
    text(xx,yy,'Nordsee',size=22.,color=tcol)

    xx,yy=proj(16.,56.5)
    text(xx,yy,'Ostsee',size=22.,color=tcol,rotation=30.)
    filename='Referenzstationen_Karte.pdf'
  else:
    #plot_station(proj,7.892,54.189,'Helgoland\n25 m')
    #plot_station(proj,8.4412,55.017,'List Koenigshafen\n10 m')
    #plot_station(proj,20.05,57.33,'Gotland Station 271\n240 m')
    #plot_station(proj,12.685,54.411,'Zingst Bay\n5 m',unsure=True)
    tcol=(1,1,1) #(0.7,0.7,0.7)(0.3,0.3,0.3)
    xx,yy=proj(1.0,56.5)
    text(xx,yy,'North Sea',size=24.,color=tcol)
    xx,yy=proj(16.,56.5)
    text(xx,yy,'Baltic Sea',size=24.,color=tcol,rotation=30.)
    
    xx,yy=proj(22.,54)
    text(xx,yy,str(tvec[i].date()),size=12.,color=(0,0,0))
   
  pos1 = gca().get_position()
  poscbar = [pos1.x0 + pos1.width/3, pos1.y0+0.18,  pos1.width/2, 0.03]
  cbaxes=f.add_axes(poscbar)
  cbartitle=unitstr
  #cbartitle=r'mmol m$^{-2}$ d$^{-1}$'
  cbaxes.set_title(cbartitle,size=12.)
  plt.colorbar(pcf, cax=cbaxes,orientation='horizontal')
  filename='png/' + varn + str(i) + '.png' #pdf
  savefig(filename,dpi=300)
  s=show()
 # close()

