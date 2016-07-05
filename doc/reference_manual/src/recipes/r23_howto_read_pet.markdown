# Recipe #23: How to read a PET log

To ensure the technical functionality of your system, please review the
the PET log after *every simulation*.

## How to get a log

The recommended practice is to set the following flags in your `mossco_run.nml`

        loglevel = 'none',
        loglevelzero = 'all',
        logflush = .true.,

## Look for ERROR first

    grep -1 ERROR PET0.log

If an error occurs, this is cause for concern, please investigate and fix this, file a
bug report or contact us.

In some cases, an ERROR is produced from features that are only recently built into ESMF.
We provide a line 'ignore this error' to indicate those situations.  You can ignore these
kinds of error, or consider upgrading your ESMF installation.

## Analysing the log file after your simulation started

Most of what can go wrong technically is apparent after the first timestep.  Thus, check
your PET file quickly after your simulation started

### Check input components

For every input component (e.g. boundaries, rivers, deposition, restarts), check that
the netCDF file is correctly read, that all variables correctly pass the include/exclude
filters, and that an ESMF_Field is created.

### Check nudge connector

Check that on those PET where it is relevant (e.g., those at the edge when boundary conditions
are used), cells are changed.

    grep cells PET0.log

### Check transport connector

    grep -1 'will transport' PET0.log

## Look for Warning

    grep -1 'WARNING'  PET0.log

Often, warnings are just that: warnings that things might have gone wrong.  Most often, you're fine.
