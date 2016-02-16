# Recipe #13: Restarting a simulation {#recipe-restarting}

Restarting a simulation with MOSSCO is a combination of two facilities

1. The implementation of the @ref `ReadRestart` ESMF phase in a component
2. Using the MOSSCO NetCDF input component

> Restarting a simulation with models that do not implement the ESMF `ReadRestart` facility, but that
have their own restarting mechanism depends on that implementation.  We provide a script to handle
this for the GETM model (`run_parallel.py`, described elsewhere).

Currently, MOSSCO supports `ReadRestart` for the benthic and pelagic ecosystem frameworks `fabm_pelagic` and `fabm_sediment`

## Adding a restart to an existing simulation

If you have already performed a simulation with output from MOSSCO and merely want to add a restart,
then you should amend your coupling `.yaml` file by

1. creating a dependency of a `restart` component to the grid you want to distribute your data on.
2. adding `restart` as an instance of the `netcdf_input` components
3. coupling  `restart` to the component that should be restarted.

For example, the following lines were added to restart the `fabm_pelagic` component.

		dependencies:
  		- restart_water:
    		- component: getm
      		grid: temperature_in_water

		instances:
  		- restart_water: netcdf_input

		coupling:
		  - components:
		     - restart_water
		     - fabm_pelagic
		    interval: none

Finally, a file `restart_water.cfg` was created with an attribute `filename:` pointing
to the output filename.  

## Using the restart facility

Edit the `mossco_run.nml` to adjust the start and stop dates to your desired time, then run the simulation.  
All components connected to the restart will then be restarted with the most recent data available in your output (depending on your configuration, but this is the default).

It is important to remove your output files (or your restart configuration file) if you want to run a restartable coupling specification from initial values (and not as a restart).

## Restarting from different grids (e.g., soil and pelagic)

Every restart (i.e., input) component can only have its data on a single grid.  Thus, the output data from the previous step has to be split into two files for the pelagic and the soil grid/input component, or, the new
`include:` specification can be used.

We tell the restart components to use this data by specifying two configuration files, that each contain a filename specification (e.g., for the gfsnrp coupling).

		echo "filename: mossco_gfsnrp.nc" > restart_soil.cfg
		echo "include:  *_in_soil" >> restart_soil.cfg
		echo "exclude: *vertical_integral*" >> restart_soil.cfg

		echo "filename: mossco_gfsnrp.nc" > restart_water.cfg
		echo "include:  *_in_water" >> restart_water.cfg
		echo "exclude: *vertical_integral*" >> restart_water.cfg
