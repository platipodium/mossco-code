# Naming convention #

MOSSCO uses the Climate and Forecast NetCDF Metadata convention. All components should add this metadata information to the variables they export to the coupled system.

A full list of currently defined CF standard names is available from [http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/26/cf-standard-name-table.html](http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/26/cf-standard-name-table.html)

## Deviations from CF convention ##

Within MOSSCO, no formal distinction is made between fresh water and sea water.  Consequently, the prefix 'sea_' in all 'sea_water_' names can be omitted

    sea_water_temperature => water_temperature



## Table of currently used CF names ##


| CF Standard Name | Units | Abbreviations/Alternates |  Explanation  |
| ------------- |:-------------:| :---| -----|
| air_pressure_at_sea_level | Pa | SLP, PSL |  
| air_temperature | | | at 2m or 3D
| cloud_area_fraction | | cloud_cover
| eastward_wind m | s-1 | air_x_velocity
| northward_wind | s-1 | air_y_velocity
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
| sea_water_x_velocity | m s-1 | water_x_velocity
| sea_water_y_velocity | m s-1 | water_y_velocity
| upward_sea_water_velocity | m s**-1 | water_z_velocity
|mole_concentration_of_particulate_organic_matter_expressed_as_phosphorus_in_sea_water| mol m-3 |
| mole_concentration_of_phytoplankton_expressed_as_phosphorus_in_sea_water | mol m-3 |
| sinking_mole_flux_of_particulate_organic_phosphorus_in_sea_water| mol m-2 s-1 |
|tendency_of_mole_concentration_of_dissolved_inorganic_phosphorus_in_sea_water_due_t| mol m-3 s-1 |



