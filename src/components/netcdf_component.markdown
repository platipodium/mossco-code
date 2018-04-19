## Component NetCDF output

The `netcdf_component` is MOSSCO's generic infrastructure for writing
data to NetCDF files.

### Description

The component writes ESMF fields that are provided to it into a netCDF file.  This component is a built-in component of MOSSCO; multiple instances of this component can be used.  

### Implementation

The NetCDF  component is realized in the file `netcdf_component.F90`. It uses
MOSSCO infrastructure from `$MOSSCO_DIR/src/utilities`, i.e. it depends on the `libmossco_utils` library, especially the `mossco_netcdf` utilities.

The NetCDF component  is an `ESMF_GridComp`.  There is no internal timestep to this
component, the clock is advanced to its `stopTime` every timestep.

Sometimes, different instances of this component are responsible for writing
different data (at different intervals) to files.

This component implements an ESMF `Run` phase that writes content it receives in its `importState` to a NetCDF file.
As a special case,  any instance of this
component is 'Run' once more than other components, to ensure that data is written **before the start** and **after the stop** of all science components.  This can result partly
in numeric output that is initialized to a non-usable value at time step zero.

### Default behavior

The content that is written is

 - attributes of the `importState` as global NetCDF attributes
 - fields and their attributes as NetCDF variables and local attributes
 - field bundles and their attirbutes as multiple NetCDF variables (indexed) and local attributes
 - states and their attributes as NetCDF variables and local attributes

All of this information will be written to the output if not specified otherwise (see below).  In addition, metadata about your system, the MOSSCO `git` sha-key, the coupling configuration and parallel layout will be written.

The file name is equal to the name of the instance.  The default name of the component is `netcdf`, thus the content is written to `netcdf.nc`.  In many of the coupling specifications found in `$MOSSCO_DIR/examples/generic/*.yaml`, the NetCDF component is renamed in the `instances:` structure, and thus the default file name is also changed accordingly.

### Configuration

Like many other components, *runtime* configuration options are accepted from a file that is named
`<instance_name>.cfg`.   The format of this file is the `ESMF_Config` file format, where labels are followed by a colon and then the value.  Tables and lists are also possible.

The NetCDF component's configuration file (e.g. `netcdf.cfg`) considers three labels

        filename: <name of a netcdf file to write to>
        checkNaN: <.false.|.true>
        include: <list of variable names to include>
        exclude: <list of variable names to exclude>

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

to obtain only variables that contain the string `DIN` but do not end in `_in_water`.  Be careful with the length of the line after any label in this ESMF configuration format, it is limited (depending on your system).

To avoid line length limitation, it is recommended to write long lists in the form of
`ESMF_Config` tables, i.e.

        include::
          chl_in_water phytoplankton*
          *at_water_surface
          *DIN*
        ::
