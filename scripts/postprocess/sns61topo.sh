rm -f topo_ur.nc topo_ll.nc topo_c.nc stitched_139x98.nc topo_c_2D.nc topo_c_3D.nc grid_139x98.nc
ncks -3 -O -v latx,lonx -d x,1,139 -d y,1,99 topo.nc topo_ur.nc
ncks -3 -O -v latx,lonx -d x,0,138 -d x,0,99 topo.nc topo_ll.nc
ncflint -O -w 0.5,0.5 -v latx,lonx topo_ur.nc topo_ll.nc topo_c.nc
ncrename -v latx,latc -v lonx,lonc topo_c.nc

ncks -3 -O -d getmGrid3D_getm_1,1,139 -d getmGrid2D_getm_1,1,139 -v getmGrid2D_getm_lon,getmGrid2D_getm_lat,getmGrid3D_getm_lon,getmGrid3D_getm_lat cut_stitched.nc grid_139x98.nc
ncks -3 -O -d getmGrid3D_getm_1,1,139 -d getmGrid2D_getm_1,1,139 -x \
  -v getmGrid2D_getm_lon,getmGrid2D_getm_lat,getmGrid3D_getm_lon,getmGrid3D_getm_lat,getmGrid3D_getm_X,getmGrid3D_getm_Y,getmGrid2D_getm_X,getmGrid2D_getm_Y cut_stitched.nc stitched_139x98.nc
ncks -3 -O -d y,0,97 topo_c.nc topo_c_2D.nc
ncks -3 -O -d y,0,97 topo_c.nc topo_c_3D.nc
ncrename -d x,getmGrid2D_getm_1 -d y,getmGrid2D_getm_2 -v latc,getmGrid2D_getm_lat -v lonc,getmGrid2D_getm_lon topo_c_2D.nc
ncrename -d x,getmGrid3D_getm_1 -d y,getmGrid3D_getm_2 -v latc,getmGrid3D_getm_lat -v lonc,getmGrid3D_getm_lon topo_c_3D.nc
ncflint -O -w 1,0 -v getmGrid2D_getm_lon,getmGrid2D_getm_lat topo_c_2D.nc grid_139x98.nc grid_2D.nc
ncflint -O -w 1,0 -v getmGrid3D_getm_lon,getmGrid3D_getm_lat topo_c_3D.nc grid_139x98.nc grid_3D.nc
ncks -3 -A -v getmGrid2D_getm_lon,getmGrid2D_getm_lat grid_2D.nc stitched_139x98.nc
ncks -3 -A -v getmGrid3D_getm_lon,getmGrid3D_getm_lat grid_3D.nc stitched_139x98.nc
ncatted -a standard_name,getmGrid3D_getm_lat,o,c,'grid_latitude' stitched_139x98.nc
ncatted -a _CoordinateAxisTypew,getmGrid3D_getm_lat,o,c,'Lat' stitched_139x98.nc
ncatted -a long_name,getmGrid3D_getm_lat,o,c,'Latitude' stitched_139x98.nc
ncatted -a units,getmGrid3D_getm_lat,o,c,'degrees_north' stitched_139x98.nc
ncatted -a standard_name,getmGrid3D_getm_lon,o,c,'grid_longitude' stitched_139x98.nc
ncatted -a _CoordinateAxisType,getmGrid3D_getm_lon,o,c,'Lon' stitched_139x98.nc
ncatted -a long_name,getmGrid3D_getm_lon,o,c,'Longitude' stitched_139x98.nc
ncatted -a units,getmGrid3D_getm_lon,o,c,'degrees_east' stitched_139x98.nc
ncatted -a standard_name,getmGrid2D_getm_lat,o,c,'grid_latitude' stitched_139x98.nc
ncatted -a _CoordinateAxisType,getmGrid2D_getm_lat,o,c,'Lat' stitched_139x98.nc
ncatted -a long_name,getmGrid2D_getm_lat,o,c,'Latitude' stitched_139x98.nc
ncatted -a units,getmGrid2D_getm_lat,o,c,'degrees_north' stitched_139x98.nc
ncatted -a standard_name,getmGrid2D_getm_lon,o,c,'grid_longitude' stitched_139x98.nc
ncatted -a _CoordinateAxisType,getmGrid2D_getm_lon,o,c,'Lon' stitched_139x98.nc
ncatted -a long_name,getmGrid2D_getm_lon,o,c,'Longitude' stitched_139x98.nc
ncatted -a units,getmGrid2D_getm_lon,o,c,'degrees_east' stitched_139x98.nc

# Do this only when you need only 2D data
ncks -3 -O -v getmGrid2D_getm_lon,getmGrid2D_getm_lat,time,water_depth_at_soil_surface,dissolved_oxygen_upward_flux_at_soil_surface stitched_139x98.nc mossco_oxysurface_20030908.nc
