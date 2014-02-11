# Naming convention #

MOSSCO uses the Climate and Forecast NetCDF Metadata convention. All components should add this metadata information to the variables they export to the coupled system.

## Deviations from CF convention ##

Within MOSSCO, no formal distinction is made between fresh water and sea water.  Consequently, the prefix 'sea_' in all 'sea_water_' names can be omitted

    sea_water_temperature => water_temperature



## Table of currently used CF names ##


| CF Standard Name | Units | Abbreviations/Alternates |  Explanation  |
| ------------- |:-------------:| :---| -----|
| air_pressure_at_sea_level | Pa | SLP, PSL |  
| air_temperature | | | at 2m or 3D
| cloud_area_fraction | | cloud_cover
| eastward_wind m | s**-1 | air_x_velocity
| northward_wind | s**-1 | air_y_velocity
| ocean_mixed_layer_thickness
| surface_temperature | K, degree_C | | 
| surface_downward_water_flux
| surface_downwelling_shortwave_flux_in_air
| surface_net_downward_shortwave_flux
| surface_downward_eastward_stress
| surface_downward_northward_stress
| sea_surface_temperature
| sea_water_potential_temperature | | sigma_temperature
| sea_water_salinity | PSU | water_salinity, salinity
| sea_water_temperature |  | water_temperature
| sea_water_x_velocity | m s**-1 | water_x_velocity
| sea_water_y_velocity | m s**-1 | water_y_velocity
| upward_sea_water_velocity | m s**-1 | water_z_velocity
