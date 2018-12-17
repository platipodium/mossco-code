from __future__ import print_function

import sys, os
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
import netCDF4
import cmocean
import pickle
from mpl_toolkits.basemap import Basemap


def plotTopo(topoFileName, parSetupFileName):
    
    dpi = 300
    
    nc=netCDF4.Dataset(topoFileName)
    
    z  = nc.variables['bathymetry'][:]
    lonx = nc.variables['lonx'][:]
    latx = nc.variables['latx'][:]

    lonb=(np.min(lonx), np.max(lonx))
    latb=(np.min(latx), np.max(latx))

    f,ax=plt.subplots((1),figsize=(8.6,8.0),dpi=dpi)
    
    basename = topoFileName.split('.nc')[0] + os.path.split(parSetupFileName)[-1]
    
    picklefile=os.path.basename(basename + '.pickle')

    if os.path.isfile(picklefile):
        (proj,)=np.load(picklefile)
    else:
        proj=Basemap(projection="merc", lat_ts=0.5*(latb[1]+latb[0]), resolution="h",llcrnrlon=lonb[0], urcrnrlon=lonb[1], llcrnrlat=latb[0], urcrnrlat=latb[1])
        f=open(picklefile,'wb')
        pickle.dump((proj,),f)
        f.close()

    lonc = 0.5*(lonx[1:,1:]+lonx[:-1,:-1])
    latc = 0.5*(latx[1:,1:]+latx[:-1,:-1])

    xc,yc = proj(lonc,latc)
    xx,yx = proj(lonx,latx)

    levels = np.percentile(z[z>0],np.linspace(0,100,21))

    #proj.pcolormesh(xc,yc,z,cmap=cmocean.cm.deep,rasterized=True,alpha=0.5)
    pcf=proj.contourf(xc,yc,z,cmap=cmocean.cm.deep,levels=levels,rasterized=True,alpha=0.4)
    plt.legend(frameon=True)
    
    proj.drawcoastlines((0.2,0.2,0.2))
    proj.fillcontinents((0.98,0.98,0.98))

    nc.close()
    
    with open(parSetupFileName,'r') as fin:
    
        ndomain = int(fin.readline())
        row = fin.readline().split()
        bounds = [int(item) for item in row]
        lines = pd.read_csv(fin,delim_whitespace=True, header=None, 
            names='index xoff yoff l ul u ur r dr d dl unknown'.split())
    
    
    for i in range(0,ndomain):
        
        #corners = (lines['dl'][i],lines['ul'][i],lines['ur'][i],lines['dr'][i],lines['dl'][i] )
        ilon = np.array([lines['xoff'][i],lines['xoff'][i], lines['xoff'][i]+bounds[0], lines['xoff'][i]+bounds[0], lines['xoff'][i]])
        ilat = np.array([lines['yoff'][i],lines['yoff'][i]+bounds[1], lines['yoff'][i]+bounds[1], lines['yoff'][i], lines['yoff'][i]])
        
        
        ilon = np.array(np.repeat(lines['xoff'][i],bounds[1]))
        ilon = np.append(ilon,np.arange(lines['xoff'][i],lines['xoff'][i]+bounds[0]))
        ilon = np.append(ilon, np.repeat(lines['xoff'][i]+bounds[0],bounds[1]))
        ilon = np.append(ilon,np.arange(lines['xoff'][i]+bounds[0],lines['xoff'][i]-1,-1))
        ilat = np.array(np.arange(lines['yoff'][i],lines['yoff'][i]+bounds[1]))
        ilat = np.append(ilat, np.repeat(lines['yoff'][i]+bounds[1],bounds[0]))
        ilat = np.append(ilat,np.arange(lines['yoff'][i]+bounds[1],lines['yoff'][i],-1))
        ilat = np.append(ilat,np.repeat(lines['yoff'][i],bounds[0]+1))
        
        ilon[ilon<0] = 0
        ilat[ilat<0] = 0
        ilon[ilon>=bounds[2]] = bounds[2]-1
        ilat[ilat>=bounds[3]] = bounds[3]-1

        ilontext = lines['xoff'][i]+0.5*bounds[0]
        ilattext = lines['yoff'][i]+0.5*bounds[1]
        if ilontext < 0: ilontext=0.5*(lines['xoff'][i]+bounds[0])
        if ilattext < 0: ilattext=0.5*(lines['yoff'][i]+bounds[1])
        ilontext = np.int(ilontext)
        ilattext = np.int(ilattext)

        try:
            proj.plot(xx[ilat,ilon],yx[ilat,ilon], 'k-')
            plt.text(xx[ilattext,ilontext],yx[ilattext,ilontext], '{}'.format(i),
                ha='center',va='center',fontsize=np.min([80/np.sqrt(ndomain),20]))
            print(i,ilontext,ilattext)
        except:
            print(i,ilontext,ilattext,ilon,ilat)
            
            #print(xx[ilon,ilat],yx[ilon,ilat])

        
    plt.title('Parallel setup for {}x{} cells in {} domains of size {}x{}, offset ({},{})'.format(
            bounds[2],bounds[3],ndomain,bounds[0],bounds[1],np.min(lines['xoff']), np.min(lines['yoff'])))
    plt.savefig('{}.png'.format(basename), dpi=dpi)
    print('Figure saved as {}.png'.format(basename))


if __name__ == '__main__':

    topoFileName = os.path.join(os.environ['MOSSCO_SETUPDIR'], 
        'sns','Topo','topo.nc')
    
    parSetupFileName = os.path.join(os.environ['MOSSCO_SETUPDIR'], 
        'sns','Parallel','par_setup.96p.9x13.dat')

    if len(sys.argv) > 1: 
        topoFileName = sys.argv[1]
    if len(sys.argv) > 2:
        parSetupFileName =sys.argv[2]
        
        
    if not os.path.exists(topoFileName):
        print('Topography file {} does not exist'.format(topoFileName))
        sys.exit(1)
        
    if not os.path.exists(parSetupFileName):
        print('Pararllel setup file {} does not exist'.format(parSetupFileName))
        sys.exit(1)
        
    plotTopo(topoFileName, parSetupFileName)
        
