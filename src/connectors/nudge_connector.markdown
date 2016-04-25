## Nudge connector

The nudge connector takes adds weighted field data from its import state
onto field data in its export state.

### Description

If the weight is zero (default), nothing is done.  If the weight exceeds its
bounds [0.0 ; 1.0] it is reset to the maximum/minimum value.

All fields in the `exportState` are checked against the `include` whitelist and
the `exclude` blacklist.  If this check is passed and the field is `complete`,
a field with the same name is searched in the `importState`.  If this is also
`complete` and if its bounds match those of the exportField, the exportField is
updated whereever both the importField and the exportField have zero or positive
values.

    exportField = (1.0 - weight) * exportField + weight * importField
    where exportField >= 0.0 and importField >= 0.0

### Implementation

The nudge connector is realized in the file `nudge_connector.F90`. It uses
MOSSCO infrastructure from `$MOSSCO_DIR/src/utilities`, i.e. it depends on the
`libmossco_utils` library.

The nudge connector is an `ESMF_CplComp`.  There is no internal timestep to this
connector, the clock is advanced to its `stopTime` every timestep.

### Configuration

The connector can be configured by the file `nudge_connector.cfg` (or a
respective configuration file upon name change of the connector).  In this file,
the following key--value pairs are recognized:

    weight:  <single real value between 0.0 and 1.0>
    include: <list of variable names to include>
    exclude: <list of variable names to exclude>

For the *weight*, a single real value is accepted.  For the *include* and
*exclude* keys, a whitespace-separated list of variable names can be given.
The default values are:

    weight: 0.0
    include: *
    exclude:

### History

The nudge connector was introduced in January 2016

### Todo

- assign different weights to different values
- map different field names
- deal with negative data
