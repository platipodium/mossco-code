#!/usr/bin/env python
# coding: utf-8

# In[1]:


from __future__ import print_function
import sys, os
import netCDF4
#from netCDF4 import Dataset, num2date, date2num
from ipywidgets import interact, interactive, fixed, interact_manual
import matplotlib.pyplot as plt
from matplotlib import dates as mdates
plt.rcParams["figure.figsize"] = (20,30)

import numpy as np
import ipywidgets as widgets
import glob
from mpl_toolkits.basemap import Basemap
import pickle


filename = os.path.join(os.environ['HOME'],'temp','test.058.nc')
if not os.path.exists(filename): sys.exit(1)

# Set up the projection
lonb=[0.1, 9.2]
latb=[51.4, 55.6]

basename = '.'.join(filename.split('.')[:-2])
picklefile=os.path.basename(basename)

if os.path.isfile(picklefile):
    (proj,)=np.load(picklefile)
else:
    proj=Basemap(projection="merc", lat_ts=0.5*(latb[1]+latb[0]), resolution="h",
        llcrnrlon=lonb[0], urcrnrlon=lonb[1], llcrnrlat=latb[0], urcrnrlat=latb[1])
    f=open(picklefile,'wb')
    pickle.dump((proj,),f)
    f.close()

f = netCDF4.Dataset(filename)

varnames = [str(key) for key in f.variables.keys()]
temporal_variables = []
for key,value in f.variables.items(): 
    if 'time' in value.dimensions: temporal_variables.extend(str(key)) 


# In[3]:


time = f.variables['time']
ntime = len(time[:])
#plt.plot(time,f.variables['speedup'])
#plt.gcf().set_size_inches((12,4))


# In[4]:


def surfplots(filename=os.path.join(os.environ['HOME'],'temp','test.058.nc'), varname='Chl_chl_in_water', n=8, tile=58):
    
    fig, ax = plt.subplots(nrows=1,ncols=n,sharex=True, sharey=True, squeeze=True,figsize=(12,4))
    f = netCDF4.Dataset(filename)
    #f = Dataset('/Volumes/mistral/decospm-20190107-o3-restartwater/mossco_ecospm.{:0>3}.nc'.format(tile))
    value = f.variables[varname]
    dims = value.dimensions
    if not 'time' == dims[0]: return
    for i in range(0,n):
        itime = np.int((i+1)*ntime/(n+1))
        if len(dims) == 4:
            ax[i].pcolor(value[itime,-1,:,:])
        elif len(dims) == 3:
            ax[i].pcolor(value[itime,:,:])
        else: continue
        ax[i].set_title(num2date(time[itime], time.units),fontsize=2*np.sqrt(n))
    
    fig, ax = plt.subplots(figsize=(12,4))
    if len(dims) == 4:
        ax.plot(num2date(time[1:], time.units),np.mean(value[1:,-1,:,:],axis=(1,2)))
    ax.set_title('Tile {} mean surface {}'.format(tile, varname))
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d %H'))

    

def Tiles(filename):
    """Reports a set of tiles that are available from a given filename"""
    
    filenames=glob.glob('.'.join(filename.split('.')[:-2])+'.*.nc')
    tiles = [int(f.split('.')[:-2]) for f in filenames]
    return set(tiles)


#interact(surfplots, varname=varnames, n=(3,10,1), tile=(0,192,1))
#plt.gcf().set_size_inches((12,4))
