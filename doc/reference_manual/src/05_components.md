


@section components Components provided by MOSSCO

Several scientific and utility components are provided by the MOSSCO system.  Currently, working utility components are:

1. netcdf: A NetCDF output component
2. netcdf_input: A NetCDF input component
3. clm_netcdf: A NetCDF input component for CLM data

Working scientific components provided by MOSSCO are

1. gotm: A component for the (external) GOTM hydrodynamic model
2. getm: A component for the (external) GET hydrodynamic model
3. erosed: A component for the (external) Delft3D erosion-sedimentation  model
4. simplewave: A wave component
5. fabm_pelagic:
6. fabm_sediment:

@subsection configuration-file MOSSCO component configuration files

Many components accept *runtime* configuration options in a file that is named 
`<component_name>.cfg`.   The format of these files is the `ESMF_Config` file format, where labels are followed by a colon and then the value.  Tables and lists are also possible.

For example, the netcdf component's configuration file `netcdf.cfg` could look as follows

		filename: /data/MOSSCO/output/myruns/01/run.nc
		exclude:  *flux*

This configuration effects that 

1. Output will then be written to the directory `/data/MOSSCO/output/myruns/01/` with the file name `run.nc` 
2. Variables that contain in their name the string `flux` will be excluded from the output.

The options that are recognized in configuration files depends on the component (see below).

Many external components accept *runtime* configuration options from namelist files (often ending with `.nml`); some models also come with other *runtime* configuration files, e.g., ending in `.inp`.  Refer to the description of the individual model or the documentation within the namelist file (available in many MOSSCO setups) for available options. 


@subsection configuration-coupling MOSSCO coupling componet configuration

Configuration options that are provided in the coupling configuration (see section on coupling configuration) take precendence over *runtime* configuration options.   Many components accept that a dependency on a grid from a different component is specified, e.g.

		dependencies:
			- netcdf_input:
				- component: getm
				- grid: temperature_in_water

This option indicates that the grid information in the `netcdf_input` component should be provided by the `getm` component. 

Another option that is supported by *all* components is the `instances:` coupling configuration option, e.g.

		instances: 
			- output1: netcdf
			- output2: netcdf

specifies that in the current coupling specification the `output1` and `output2` components are both instances of the `netcdf` component.  Thus, it is possible to direct output of different couplings into different outputs.

@subsection netcdf-component NetCDF output utility component

The component `netcdf` (source code in `$MOSSCO_DIR/src/components/netcdf_component`) is a utility output for writing field data that is available in the coupled system to a NetCDF file.  


Netcdf | General | Details
-- | -- | --
Dependencies | libnetcdf, libnetcdff | system-provided NetCDF Fortran library
Location Source | `$MOSSCO_DIR/src/components/` | `netcdf_component.F90`
Configuration file | `netcdf.cfg` |
Configuration option | `filename:` | name of a netCDF file to write to
Configuration option | `exclude:` | exclude pattern for variables *not* to write out
Coupling name | `netcdf` | 
Coupling option | `instance:` | run this component as an instance under a different name

If you rename the component with the `instance:`, then also the default name of the configuration file is change according to the name specified by `instance:`.   And -- if no `filename:` configuration option is provided, also the default output name changes to the name of the `instance:`.  E.g., specifying

		instances:
			- output1: netcdf
			
would read the file `output1.cfg` for its configuration and without specification of a `filename:` option would direct its output to the file `output1.nc`.  

@subsection netcdf-component NetCDF input utility component

The component `netcdf_input` (source code in `$MOSSCO_DIR/src/components/netcdf_input_component`) is a utility input for reading field data that is available in the coupled system to a NetCDF file. 


Netcdf | General | Details
-- | -- | --
Dependencies | libnetcdf, libnetcdff | system-provided NetCDF Fortran library
Location Source | `$MOSSCO_DIR/src/components/` | `netcdf_input_component.F90`
Configuration file | `netcdf_input.cfg` |
Configuration option | `filename:` | name of a netCDF file to read from
Configuration option | `grid:` | grid onto which to read the data
Coupling name | `netcdf_input` | 
Coupling option | `instance:` | run this component as an instance under a different name

This component currently needs grid information from another component and can only read data that is already represented on this grid (this restriction will be dropped in the near future).  

