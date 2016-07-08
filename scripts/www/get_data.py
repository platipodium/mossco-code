# This script fetches data from an opendap server and saves it locally
# as a limited dataset

import os, sys, datetime
import numpy as np
import netCDF4
import urllib
#import matplotlib.pyplot as plt
#from pylab import *

__author__   = 'Carsten Lemmen'
__email__    = 'carsten.lemmen@hzg.de'
__created__  = datetime.datetime(2016, 7, 7)
__modified__ = datetime.datetime(2016, 7, 8)
__status__   = "Development"


datasets = {
# It is required to specify a 'url' and a 'var' field to tell
# what variable to extract from which dataset
# Optional are 'lat' and 'lon' to specify the names of lat and lon
# coordinates, and 'service' to specify dods or ncss
    'etopo1': {
      'url' : 'http://geoport.whoi.edu/thredds/dodsC/bathy/etopo1_bed_g2',
      'var' : 'topo'
    },
    'esacci-daily': {
      'url' : 'https://rsg.pml.ac.uk/thredds/ncss/CCI_ALL-v2.0-DAILY',
      'var' : 'chlor_a',
      'service' : 'ncss'}
}


regions = {
    'sns': [0, 50, 10, 58],
}


def get_url_bounds(dataset, corners):

    if dataset.has_key('service'): service=dataset['service']
    else: service='dods'

    timevar='time'
    variables=''    
    
   
    if (service == 'dods'): fullUrl=get_dods_url(dataset, corners)
    if (service == 'ncss'): fullUrl=get_ncss_url(dataset, corners)
        
    fileName = os.path.split(dataset['url'])[-1] + '.nc'
    string = 'wget -O ' + fileName + ' ' +  fullUrl
     
    print string 
    print 'https://rsg.pml.ac.uk/thredds/ncss/CCI_ALL-v1.0-DAILY?north=58&west=0&east=10&south=52&disableProjSubset=on&horizStride=1&time_start=1997-09-04T00%3A00%3A00Z&time_end=2012-07-31T00%3A00%3A00Z&timeStride=1&addLatLon=true'
        

def get_dods_url(dataset, corners):

    nc = netCDF4.Dataset(dataset['url'])    
    if dataset.has_key('lon'): lonvar=dataset['lon']
    else: lonvar='lon'
    
    if dataset.has_key('lat'): lonvar=dataset['lat']
    else: latvar='lat'

    if lonvar in nc.variables.keys(): 
        lon=nc.variables[lonvar][:]
        inLon=np.where(lon >= corners[0])[0]
        inLon=inLon[np.where(lon[inLon] <= corners[2])[0]]
        inLonString='[' + str(inLon[0]) + ':1:' + str(inLon[-1]) + ']'
        variables=variables + lonvar + inLonString
        
        
    if latvar in nc.variables.keys(): 
        lat=nc.variables[latvar][:]
        inLat=np.where(lat >= corners[2])[0]
        inLat=inLat[np.where(lat[inLat] <= corners[3])[0]]
        inLatString='[' + str(inLat[1]) + ':1:' + str(inLat[-1]) + ']'
        variables=variables + ',' + latvar + inLatString
        
        
    if timevar in nc.variables.keys():
        #time=nc.variables[timevar][:]
        #ntime=len(time)
        inTimeString='[0:1:1]'
        variables=variables + ',' + timevar + inTimeString
         
    
    return dataset['url'] +  '.dods?' + variables
    
def get_ncss_url(dataset, corners):

    fullUrl = dataset['url'] + '?'
    urlAttributes = { 'var' : dataset['var'], 'north': corners[3],  'west' : corners[0],
        'south' : corners[1], 'east' : corners[2], 'disableProjSubset' : 'on',
        'horizStride' : 1, 'time_start' : '2012-07-04T00:00:00Z' ,
        'time_end' : '2012-07-31T00:00:00Z', 'timeStride' : 1, 
        'accept' : 'netCDF', 'addLatLon' : 'true'
    }  
    
    return fullUrl + urllib.urlencode(urlAttributes)

if __name__ == '__main__':

    if len(sys.argv) > 3: dataset=sys.argv[2]
    else: dataset = 'esacci-daily' #'etopo1'

    if len(sys.argv) > 2: region=sys.argv[1]    
    else: region = 'sns'
        
    #os.mkdir(dataset + '/' + region)
    
    if datasets.has_key(dataset) and regions.has_key(region): 
        get_url_bounds(datasets[dataset],regions[region])


    