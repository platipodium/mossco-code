
@subpage netcdf-component NetCDF component

The NetCDF component writes Fields that are provided to it into a netCDF file.  This component is a built-in component of MOSSCO; multiple instances of this component can be used.  

@section netcdf-component-default-behavior  Default behavior

This component implements an ESMF `Run` phase that writes content it receives in its `importState` to a NetCDF file.

The content that is written is

 - attributes of the `importState` as global NetCDF attributes
 - fields and their attributes as NetCDF variables and local attributes
 - field bundles and their attirbutes as multiple NetCDF variables (indexed) and local attributes
 - states and their attributes as NetCDF variables and local attributes
 
All of this information will be written to the output if not specified otherwise (see below).  In addition, metadata about your system, the MOSSCO `git` sha-key, the coupling configuration and parallel layout will be written.
 
The file name is equal to the name of the component.  The default name of the component is `netcdf`, thus the content is written to `netcdf.nc`.  In many of the coupling specifications found in `$MOSSCO_DIR/examples/generic/*.yaml`, the NetCDF component is renamed in the `instances:` structure, and thus the default file name is also changed accordingly.

@section netcdf-component-configuration-file Configuration files

Like many other componets, *runtime* configuration options are accepted from a file that is named 
`<component_name>.cfg`.   The format of this file is the `ESMF_Config` file format, where labels are followed by a colon and then the value.  Tables and lists are also possible.

The NetCDF component's configuration file (e.g. `netcdf.cfg`) considers three labels

 1. `filename:`
 2. `exclude:`
 3. `include:`
 
With the `filename:` label, you can change the name and location of the output file, i.e.

		filename: /my-data/MOSSCO/mossco-output/my-simulation/my-output-filename.nc
		
would order the NetCDF component to write the output to the file ` /my-data/MOSSCO/mossco-output/my-simulation/my-output-filename.nc`.  If not given, then the default output is `<component_name>.nc` in your local directory.  If the file cannot be created, an error is created and the simulation is stopped.

The `include:` and `exclude:` labels accept a white space-separated list of field names that are either included or excluded from the output.  Field names can be partically given and prefixed or postfixed with the `*` wildcard to allow arbitrary characters before or after the specified pattern.

The default for inclusion is the `*` wildcard, i.e. all fields are included.

		include: chl_in_water phytoplankton* *at_water_surface *DIN*

The above pattern would write the field `chl_in_water`, all fields starting with `phytoplankton`, all fields ending with `at_water_surface`, and all fields containing `DIN` to the output file.

		exclude: chl_in_water phytoplankton* *at_water_surface *DIN*
		
The above pattern would write all fields except the ones specified with this pattern.		
Exclusion overrides inclusion, e.g., you can specify the following:

		include: *DIN*
		exclude *_in_water
		
to obtain only variables that contain the string `DIN` but do not end in `_in_water`.

