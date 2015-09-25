#!/usr/bin/env python
#> @brief script for making 2-D plots of getm 3-D variables

#  This computer program is part of MOSSCO.
#> @copyright Copyright (C) 2015 Helmholtz Zentrum Geesthacht
#> @author Onur Kerimoglu <onur.kerimoglu@hzg.de>, Richard Hofmeister <richard.hofmeister@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

""" module plot_mossco2Dmap.py
example call from shell:

$ python plot_mossco2Dmap.py filename.nc 'var1','var2' day1,day2,day3 method

makes 2-D maps of filename.nc for var1,var2(,etc) and every day1,day2(,etc) of each month,
where vars are expected to have 3 (time,lon,lat) or 4 dimensions (time,layer,lon,lat).
-3D: A 2-D plot will be made for each specified time
-4D, method='each': A 2-D plot will be made for each specified time & available layer
-4D, method='int': A 2-D plot of water column integral will be made for each specified time
"""

import numpy as np
import netCDF4,pickle,netcdftime,os,re,sys
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap


def do_2Dplotmap(fname, varnames,timeint,setup,method):

    proj=getproj(setup)

    # read the nc-file
    #nc=netCDF4.Dataset('../Topo/NSBS6nm.v01.nc')
    nc=netCDF4.Dataset(fname)
    #nc=netCDF4.Dataset('maecsomexdia_soil_stitched.nc')

    ncv=nc.variables

    #common variables
    try:
        #if spherical coordinates:
        #LO = ncv['getmGrid2D_getm_x'][:]
        #LA = ncv['getmGrid2D_getm_y'][:]

        lon=ncv['lonc'][:]
        lat=ncv['latc'][:]
        LO,LA = np.meshgrid(lon,lat)
        x,y = proj(LO,LA)

    except:
        #raise 'could not find getmGrid2D_getm_lon and/or getmGrid2D_getm_lat dimensions'
        #for now, support lon_2/lat_2 coordinates
        print 'getmGrid2D_getm_lon and/or getmGrid2D_getm_lat were not found. Will look for lon_2/lat_2:',
        try:
            lon = ncv['lon_2'][:]
            lat = ncv['lat_2'][:]
            print ' found.'
            LO,LA = meshgrid(lon,lat)
            x,y = proj(LO,LA)
        except:
            print ' failed.'
            raise 'could not find lat/lon dimensions'


    tv = nc.variables['time']
    utime=netcdftime.utime(tv.units)
    tvec=utime.num2date(tv[:])
    #days to plot:
    days=[tvec[ti].day for ti in range(0,len(tvec))]
    #(tind,)=np.where(np.in1d(days,timeint))
    tind=[12,13,14]

    for varno,varname in enumerate(varnames):
        print varname,

        v = ncv[varname][:]
        unitstr=ncv[varname].units
        longname=ncv[varname].long_name.replace('_', ' ')

        if re.search('water',varname):
            print ': identified as pelagic variable. plotting scene#:',
            vardom='water'
        elif re.search('soil',varname):
            print ': identified as soil variable. plotting scene#:',
            vardom='soil'

        for i in tind:
            print str(i),

            if len(v.shape)==3: #directly plot the variable
                vI=v[i,:,:]
                suffix=''
                filename=fname.split('.nc')[0]+'_'+varname+suffix+'_'+ str(tvec[i].date()) + '.png' #pdf
                titlestr=suffix+' '+longname+'\n'+str(tvec[i].date())
                plot2Dmap(x,y,vI,proj,setup,filename,titlestr,cbarstr=unitstr)

            elif len(v.shape)==4 and (method=='int' or method=='avg'):
                #calculate vertical integral or average
                lh=nc.variables['h'][:] #layer thickness
                vI=0;lhI=0
                for k in range(1,v.shape[1]):
                    #layer height of the corresponding domain
                    lhI=lhI+lh[i,k,:,:]
                    vI=vI+v[i,k,:,:]*lh[i,k,:,:]
                if method=='avg':
                    vI=vI/lhI
                    suffix='average'
                elif method=='unweighted-average':
                    suffix='unweighted-average'
                elif method=='int': #divide by total height
                    suffix='integral'
                    unitstr.replace('m**3','m**2')

                filename=fname.split('.nc')[0]+'_'+varname+'-'+suffix+'_'+ str(tvec[i].date()) + '.png' #pdf
                titlestr=suffix+' '+longname+'\n'+str(tvec[i].date())
                plot2Dmap(x,y,vI,proj,setup,filename,titlestr,cbarstr=unitstr)

            elif len(v.shape)==4 and method=='each':
                lev=ncv['level'][:]
                levind=[0,1]
                for k in levind:
                    vI=v[i,k,:,:]
                    suffix='L'+str(levind[k])
                    suffixtit=suffix+'('+str(lev[k])+')'
                    print '-L'+str(levind[k]),

                    filename=fname.split('.nc')[0]+'_'+varname+'-'+suffix+'_'+ str(tvec[i].date())+ '.png' #pdf
                    titlestr=suffixtit+' '+longname+'\n'+str(tvec[i].date())
                    plot2Dmap(x,y,vI,proj,setup,filename,titlestr,cbarstr=unitstr)

        print '.'

    #close the netcdf file
    nc.close()

def plot2Dmap(x,y,v,proj,setup,filename,titlestr,cbarstr):
    f = plt.figure(figsize=(10,6), dpi=96)
    f.subplots_adjust(left=0.0,right=1.0,bottom=0.0,top=.9)

    #plot
    #proj.contourf(x,y,h,levels=range(0,255,25),extend='both',cmap=cm.YlGnBu)
    #proj.contourf(x,y,h[i,:,:],extend='both',levels=np.arange(0.0,1.0,0.1),cmap=cm.YlOrRd) #cm.YlGnBu
    clim=[np.amin(v),np.amax(v)] #[np.amax(v)*.8,np.amax(v)] #
    pcf=proj.pcolormesh(x,y,v,cmap=plt.get_cmap('jet'),vmin=clim[0], vmax=clim[1]) #'YlOrRd'

    plt.title(titlestr)

    #setup specific features
    if setup in ['NSBS','NSBSfull','SNS']:
        #coastlines, etc
        #proj.drawcoastlines(color=(0.7,0.7,0.7),linewidth=0.2)
        #proj.fillcontinents((0.7,0.7,0.7),lake_color=(0.9,0.9,1.0))
        proj.drawcoastlines(color=(0.3,0.3,0.3),linewidth=0.5)
        proj.fillcontinents((1.0,1.0,1.0),lake_color=(0.9,0.9,1.0))
        plt.gca().patch.set_facecolor((0.9,0.9,1.0))
        #retrieve the axes position to set the colorbar position
        pos1 = plt.gca().get_position()

    if setup=='NSBS':
        #some annotation
        #mark_stats(proj, lang='en', stations=True, seas=True)
        #colorbar
        poscbar = [pos1.x0 + pos1.width/3+0.1, pos1.y0+0.18,  pos1.width/2, 0.03]
        cbaxes=f.add_axes(poscbar)
        plt.colorbar(pcf, cax=cbaxes,orientation='horizontal')
    elif setup=='NSBSfull':
        #some annotation
        #mark_stats(proj, lang='en', stations=True, seas=True)
        #colorbar
        poscbar = [pos1.x0 + pos1.width/3, pos1.y0+0.08,  pos1.width/2, 0.03]
        cbaxes=f.add_axes(poscbar)
        plt.colorbar(pcf, cax=cbaxes,orientation='horizontal')
    elif setup=='SNS':
        #colorbar
        poscbar = [pos1.x0 + pos1.width/2+0.1, pos1.y0+0.18,  pos1.width/2.5, 0.03]
        cbaxes=f.add_axes(poscbar)
        plt.colorbar(pcf, cax=cbaxes,orientation='horizontal')
    elif setup=='deep_lake':
        #colorbar
        cbaxes=plt.colorbar(pcf)

    #set colorbar title
    cbaxes.set_title(cbarstr,size=12.)

    plt.savefig(filename,dpi=300)
    #s=show()
    plt.close(f)

def getproj(setup):

    if os.path.isfile('proj.'+setup+'.pickle'):
        print 'opening an existing projection: '+ 'proj.'+setup+'.pickle'
        #if a projection exists, just load it (fast)
        (proj,) = np.load('proj.'+setup+'.pickle')
    else:
        print 'projecting for: '+ setup
        if setup=='NSBS':
            # initialise geographic projection (slow)
            proj=Basemap(projection='lcc',
                   resolution='i',
                   llcrnrlon=-1.0,
                   llcrnrlat=50.5,
                   urcrnrlon=26.0,
                   urcrnrlat=59.5,
                   lat_0=54.0,
                   lon_0=20.)
        elif setup=='NSBSfull':
            # initialise geographic projection (slow)
            proj=Basemap(projection='lcc',
                   resolution='i',
                   llcrnrlon=-4.0,
                   llcrnrlat=48.0,
                   urcrnrlon=32.5,
                   urcrnrlat=66.0,
                   lat_0=54.0,
                   lon_0=20.)
        elif setup=='SNS':
                proj=Basemap(projection='lcc',
                       resolution='i',
                       llcrnrlon=0.5,
                       llcrnrlat=52.5, #51
                       urcrnrlon=9.5,
                       urcrnrlat=57.5,
                       lat_0=52.0,
                       lon_0=5.)
        elif setup=='deep_lake':
            proj=Basemap(projection='lcc',
                         resolution='i',
                         llcrnrlon=-7.0,
                         llcrnrlat=47.8,
                         urcrnrlon=-4.0,
                         urcrnrlat=49.4,
                         lat_0=48.0,
                         lon_0=-5.0)
        else:
            raise 'unknown setup. cannot produce projection.'

        #pickle for later use:
        f=open('proj.'+setup+'.pickle','wb')
        pickle.dump((proj,),f) #,protocol=-1
        f.close()

    return proj

def mark_stats(proj, lang='en', stations=False, seas=True):
    if lang=='de':
        if stations:
            plot_station(proj,7.892,54.189,'Helgoland Reede\n25 m')
            plot_station(proj,8.073,54.061,'NOAH Station C',right=True)
            plot_station(proj,8.4412,55.017,'List Koenigshafen\n10 m')
            plot_station(proj,20.05,57.33,'Gotland Station 271\n240 m')
            plot_station(proj,12.685,54.411,'Zingster Bodden\n5 m',unsure=True)
        if seas:
            xx,yy=proj(1.0,56.5)
            plt.text(xx,yy,'Nordsee',size=18.,color=(0.3,0.3,0.3))
            xx,yy=proj(16.,56.5)
            plt.text(xx,yy,'Ostsee',size=18.,color=(0.3,0.3,0.3),rotation=30.)
            filename='Referenzstationen_Karte.pdf'
    elif lang=='en':
        if stations:
            plot_station(proj,7.892,54.189,'Helgoland\n25 m')
            plot_station(proj,8.4412,55.017,'List Koenigshafen\n10 m')
            plot_station(proj,20.05,57.33,'Gotland Station 271\n240 m')
            plot_station(proj,12.685,54.411,'Zingst Bay\n5 m',unsure=True)
        if seas:
            xx,yy=proj(1.0,56.5)
            plt.text(xx,yy,'North Sea',size=18.,color=(0.3,0.3,0.3))
            xx,yy=proj(16.,56.5)
            plt.text(xx,yy,'Baltic Sea',size=18.,color=(0.3,0.3,0.3),rotation=30.)


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

        plt.text(xx+xoffset,yy-yoffset,name,horizontalalignment='left', \
            verticalalignment='top',size=10,color=color)
    else:
        xoffset,yoffset=(25000,10000) # m

        plt.text(xx-xoffset,yy+yoffset,name,horizontalalignment='right', \
            verticalalignment='top',size=10,color=color)

if __name__=='__main__':
    if len(sys.argv)>1:
        fname=sys.argv[1]
    else:
        fname='/home/onur/WORK/projects/NSBS/setups/mossco/NSBS/knut/getm/getm2d.nc'
        #fname='/home/onur/WORK/projects/NSBS/setups/mossco/NSBS/knut/getm/getm3d.nc'

    if len(sys.argv)>2:
        varnames=sys.argv[2].split(',')
    else:
        varnames=['waveH','waveL']
                 #'temp','salt']
        #varnames=['denitrification_rate_in_soil', 'dissolved_oxygen_in_soil']

    if len(sys.argv)>3:
        timeint=map(int, sys.argv[3].split(','))
    else:
        timeint=[1,15] #i.e, plot on every 1st and 15th of each month

    if len(sys.argv)>4:
        setup=sys.argv[4]
    else:
        setup='NSBS'
        #setup='deep_lake'

    if len(sys.argv)>5:
        method=sys.argv[5]
    else:
        method='avg' #'avg','int','each'
        #setup='deep_lake'

    do_2Dplotmap(fname,varnames,timeint,setup,method)
