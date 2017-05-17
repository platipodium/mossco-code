## Component time aggregation

The `time_aggregation_component` is MOSSCO's generic temporal aggregation component.

### Description

The component aggregates ESMF fields that are provided to it and delivers this
aggregation back upon coupling thereby resetting the aggregated fields. This component is a built-in component of MOSSCO; multiple instances of this component can be used.  

### Implementation

The time aggregation component is realized in the file `time_aggregation_component.F90`. It uses
MOSSCO infrastructure from `$MOSSCO_DIR/src/utilities`, i.e. it depends on the `libmossco_utils`.

The time aggregation component  is an `ESMF_GridComp`.  There is no internal timestep to this
component, the clock is advanced to its `stopTime` every timestep.

Often, different instances of this component are responsible for aggregating
different data (at different intervals).

This component implements an ESMF `Run` phase that aggregates content it receives in its `importState` to its `exportState`, prefixing the variable name with 'avg_'.  Whenever
the component is coupled to another component, the aggregated data in exportState are
reset (after delivery) to start a new aggregation.

### Configuration

Like many other components, *runtime* configuration options are accepted from
a file that is named `<instance_name>.cfg`.   The format of this file is the
`ESMF_Config` file format, where labels are followed by a colon and then
the value.  Tables and lists are also possible.

The time aggregation component's configuration file (e.g.
`time_aggregation.cfg`) considers three labels

        include: <list of variable names to include>
        exclude: <list of variable names to exclude>

The `include:` and `exclude:` labels accept a white space-separated list of field names that are either included or excluded from the output.  Field names can be partially given and prefixed or postfixed with the `*` wildcard to allow arbitrary characters before or after the specified pattern.

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

## Limitations

The component does only handle 2D and 3D fields, not field bundles.
