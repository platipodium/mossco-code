# Recipe #12: Including rivers

Rivers can be added to a MOSSCO coupled system just like any other NetCDF input with by coupling to the `netcdf_input` component, provided that the input file conforms to the grid that is used by the component that requires this input. 


## A sample coupling configuration

For the purpose of this example, we will read in the file `river_grid_fluxes.nc` with the `river_input` component running as an instance of `netcdf_input`.  The grid will be obtained from the `getm` component.

Thus, in the coupling configuration, we specifiy the grid dependency and name the instance

	dependencies:
  		- river_input:
    		- component: getm
      		- grid: temperature_in_water

	instances:
  		- river_input: netcdf_input
  		
	coupling:
		- components:
			- river_input
			- fabm_pelagic
		- interval: 1 d 

Then the configuration file `river_input.cfg` contains the name of the file to read

		filename: /my/path/to/input/files/river_grid_fluxes.nc
		
