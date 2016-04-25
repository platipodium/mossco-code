## NeCDF input component

The NetCDF input component is MOSSCO's generic infrastructure for reading
data from NetCDF files.

### Description

The component reads all or a set of variables from a NetCDF file and provides
these data as ESMF fields to a coupled system.  The component only delivers a
single time slice of the entire data set, if there is a time dimension.


### Implementation

The NetCDF input component is realized in the file `netcdf_input_component.F90`. It uses
MOSSCO infrastructure from `$MOSSCO_DIR/src/utilities`, i.e. it depends on the
`libmossco_utils` library, especially the `mossco_netcdf` utilities.

The NetCDF input component  is an `ESMF_GridComp`.  There is no internal timestep to this
component, the clock is advanced to its `stopTime` every timestep.

Most often, different instances of this component are responsible for reading
different data files.

The component relies on a foreign grid delivered at initialize time.  This
requirement can be met by specifying a suitable `dependencies` entry in the
coupling configuration.

### Configuration

The component can be configured by the file `netcdf_input.cfg` (or a
respective configuration file upon name change of the component).  In this file,
the following key--value pairs are recognized:

    filename: <name of a file to read from>
    include: <list of variable names to include>
    exclude: <list of variable names to exclude>
    alias: <key=value list of mappings from netcdf to mossco field names>
    interpolation: <named method for interpolation>
    climatology: <time interval covered by the climatology>

- The `filename:` configuration option is mandatory.  If not given, no data is read; if
the file does not exist, no data is read either.

- The `include:` whitespace-separated list provides patterns for field names that
are read exclusively. The default is not set, i.e. all variables are read.  The
pattern can use the asterisk `*` at the beginning and/or end for matching any
positive number of characters.

- The `exlude:` whitespace-separated list provides patterns for field names that are excluded
from reading (after limitations by the `include:` configuration).  The default is
"none", i.e. all variables are read.

- The `alias:` whitespace-separated key=value list provides mappings of the form
ncvarname=fieldname to translate between NetCDF variable names and field names
used within MOSSCO. The default is no translation.

- The `climatology:`  is whitespace-separated string of a number and an ESMF
time unit  (i.e., `yy, mm, d, h, m, s`).  It denotes the length of the climatological
period (beginning with the first time in the data) that should be repeatedly used.

- The `interpolation`: string is one of "recent, nearest, next, linear". The first three
report the data at the nearest recent, nearest, or nearest next time data point,
the latter interpolates linearly between the two nearest time data points.

### History

The NetCDF input component was introduced in June 2014
- climatology was added in January 2016
- interpolation was added in February 2016

### Todo

- read grid from `SCRIP` or `GRIDSPEC` formats
- deal with meshes and location streams
- should this be a mediator?
