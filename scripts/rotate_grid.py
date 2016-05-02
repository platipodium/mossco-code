# see http://gis.stackexchange.com/questions/10808/lon-lat-transformation for source
# of this script, oritinal author @simondk

import numpy as np
import netCDF4

__author__ = 'Carsten Lemmen'


def rotate(rotated_lonlat, (rotated_southpole_lon, rotated_southpole_lat) = (0,0)):

    if np.all(rotated_lonlat < 1):
        print('Please give coordinates in degrees, not radians')


    rlon = rotated_lonlat[0]
    rlat = rotated_lonlat[1]


    theta = 90 + rotated_southpole_lat  # Rotation around y-axis
    phi = rotated_southpole_lon       # Rotation around z-axis

    phi = (phi*np.pi)/180
    theta = (theta*np.pi)/180

    x = np.cos(rlon) * np.cos(rlat) # Convert from spherical to cartesian coordinates
    y = np.sin(rlon) * np.cos(rlat)
    z = np.sin(rlat)

    phi = -phi
    theta = -theta

    x_new = np.cos(theta)  * np.cos(phi) * x + np.sin(phi) * y + np.sin(theta) * np.cos(phi) * z
    y_new = -np.cos(theta) * np.sin(phi) * x + np.cos(phi) * y - np.sin(theta) * np.sin(phi) * z
    z_new = -np.sin(theta) * x + np.cos(theta) * z


    lon = np.arctan2(y_new,x_new) #  Convert cartesian back to spherical coordinates
    lat = np.arcsin(z_new)

    lon = (lon * 180) / np.pi  # Convert radians back to degrees
    lat = (lat * 180) / np.pi

    return [lon, lat]


if True:
    filename = '/Volumes/Kea/data/clm/test.nc'

    nc = netCDF4.Dataset(filename,'r')
    grlon=nc.variables['rlon'][:]
    grlat=nc.variables['rlat'][:]

    rlon=np.reshape(grlon,(len(grlon),1))
    rlat=np.reshape(grlat,(1,len(grlat)))
    rlon=np.repeat(rlon,len(grlat), axis=1)
    rlat=np.repeat(rlat,len(grlon), axis=0)
    
    rlon=np.squeeze(np.reshape(rlon,(len(grlat)*len(grlon),1)))
    rlat=np.squeeze(np.reshape(rlat,(len(grlat)*len(grlon),1)))

    rotation=nc.variables['rotated_pole']
    
    [lon,lat] = rotate([rlon,rlat], (180.0 - rotation.grid_north_pole_longitude, -rotation.grid_north_pole_latitude))
    
    print lon.shape, lat.shape



