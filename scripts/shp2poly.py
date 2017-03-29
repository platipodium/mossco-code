# -*- coding: utf-8 -*-
"""
Created on Fri Jun 20 12:28:35 2014

@author: lemmen
"""


import shapefile

sf = shapefile.Reader("/Volumes/Kea/data/Kaela//KS_062016_Master/GIS/EEZ/NS_EEZ.shp")

#shapefile.Reader("/Users/lemmen/devel/data/Helgoland/12632341/12632341")
shapes = sf.shapes()
bbox=shapes[0].bbox
geometry = sf.shapeRecords() #will store the geometry separately
first = geometry[0] #will extract the first polygon to a new object
first.shape.points #will show you the points of the polygon
first.record #will show you the attributes

#[-122.485792, 37.786931000000003, -122.446285, 37.811019000000002]
#>>>#  Read the 8th point in the 4th shape
#>>> shapes[3].points[7]
#[-122.471063, 37.787402999999998]
