# CLM NetCDF component

This component communicates information from the Climate Local Model (CLM) available in NetCDF format from the CoastDat2 database http://www.coastdat.de/data_all/index.php to a coupled system.


## Source files
This component is implemented in `$MOSSCO_DIR/src/components/clm_netcdf_component.F90`.  The component relies on a driver file located at `$MOSSCO_DIR/src/drivers/clm_netcdf_driver.F90`.  Both sources are compiled into a library `$MOSSCO_DIR/libraries/<your-compiler>/libmossco_clm.a`.


## Configuration files

### `atmos.rc`: Grid and processor layout
This file contains information about the layout of the atmospheric module on the available processors. "iprocs" is the number of processors along the x-axis (i.e. constant latitude), and "jprocs" is the number of PEs along the y-axis (constant longitude). The product iprocs*jprocs MUST match the total number of processors declared when starting mpirun.

This file could look as follows:

	# Resource file for atmosphere
	# Decomposition layout
	iprocs:      1
	jprocs:      1

### `config.rc`: NetCDF information

* PLEASE BE AWARE THAT THIS CONFIG FILE IS NOT FULLY FUNCTIONAL YET AND SERVES AS AN EXAMPLE ONLY.*
 
It contains information about the content of the CLM atmospheric data files which is generated from an `NF90_INQUIRE` call augmented with some switches and name tags and also gives the netcdf-names and -longnames. The legend is given in the header of the file. "nvar" is self-explaining. The flag "ACT" allows a user of the module to select the variables to be used as well as their ESMF_Field names in the column "NAME". A scale factor can be given in the column labelled "SCALE".

This file (partly) looks as follows:

	# Number of variables
	nvar:   24

	#ID NAME ACT SCALE NDIMS CDF-NAME      LONGNAME
	1: NONE  0  1.00   3    ASWDIFU_S     diffuse upwnward sw radiation at the surface [W m-2]
	2: NONE  0  1.00   3    ASWDIR_S      direct downward sw radiation at the surface [W m-2]
	3: NONE  0  1.00   3    ATHB_S        averaged surface net downward longwave radiation [W m-2]
	4:   CC  1  1.00   3    CLCT          total cloud cover [1]
	5:  PSL  1  0.01   3    PMSL          mean sea level pressure [Pa]



## Input files

### `clm_grid.nc`: SCRIP grid description
This is the file specifying the CLM grid structure in SCRIP format for the input file `atmos.nc` (see below). For details on the SCRIP format for structured grids see the ESMF_6.3.0 Reference Manual in Chapter 12.4.

This file could look similar to the following (output from `ncdump -h clm_grid.nc`)

	netcdf clm_grid {
	dimensions:
	  grid_size = 4 ;
	  grid_corners = 4 ;
	  grid_rank = 2 ;
	variables:
	  int grid_dims(grid_rank) ;
	  double grid_center_lat(grid_size) ;
		grid_center_lat:units = "degrees" ;
	  double grid_center_lon(grid_size) ;
		grid_center_lon:units = "degrees" ;
	  int grid_imask(grid_size) ;
		grid_imask:units = "unitless" ;

	// global attributes:
	  :title = "CLM sample grid" ;
	}

See this information from the ESMF Manual on [SCRIP](http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_6_3_0r/ESMF_refdoc/node3.html#SECTION03024000000000000000)


### `atmos.nc`: Atmospheric data file
This file contains the atmospheric data.  You can obtain this data file from the CoastDat database located at [http://www.coastdat.de/data_all/index.php](http://www.coastdat.de/data_all/index.php) .

