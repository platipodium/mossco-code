# Recipe #13: Restarting a simulation

Restarting a simulation with MOSSCO is a combination of two facilities

1. The implementation of the `ReadRestart` ESMF phase in a component
2. Using the MOSSCO NetCDF input component

Restarting a simulation with models that do not implement the ESMF `ReadRestart` facility, but that 
have their own restarting mechanism depends on that implementation.  We provide a script to handle
this for the GETM model (described elsewhere).

Currently, MOSSCO supports `ReadRestart` for the benthic and pelagic ecosystem frameworks `fabm_pelagic` and `fabm_sediment`


## A base simulation

For this recipe, we use the `$MOSSCO_SETUPDIR/deep_lake` setup and the `gffrr` coupling specification (containing a restartable coupling of getm--fabm_pelagic--fabm_sediment--river components).

Change to the `$MOSSCO_SETUPDIR/deep_lake` directory and issue a `make clean`, then build the example

		cd $MOSSCO_SETUPDIR/deep_lake
		make clean
		mossco -rb  gffrr
		
It is important to issue `make clean` if you want to run a restartable coupling specification from initial values (and not as a restart).  Edit the `mossco_run.nml` to adjust the start and stop dates to your desired time, then run the simulation for the first time.

		mossco -n1 gffrr

Output will be collected in the file `mossco_gffrr.nc`. 

## The restarted simulation

Every restart (i.e. input) component can only have its data on a single grid.  Thus, the output data from the previous step has to be split into two files for the pelagic and the soil grid/input component.  Here, we do this with the NetCDF Operator `ncks`

		ncks -v .*_in_soil mossco_gffrr.nc restart_soil.nc
		ncks -v .*_in_water mossco_gffrr.nc restart_water.nc

You should  use the `-d time,<index>` option to ncks to select only one point in time that will be used for reinitializing your state variables.
 
We tell the restart components to use this data by specifying two configuration files, that each contain a filename specification.

		echo "filename: restart_soil.nc" > restart_soil.cfg
		echo "filename: restart_water.nc" > restart_water.cfg
		
Now rerun your simulation

		mossco -n1 gffrr

FABM pelagic and benthic variables will be reinitialized with the values in `restart_soil.nc` and `restart_water.nc` at simulation start.
