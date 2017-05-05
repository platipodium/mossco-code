# Recipe #23: How to read a PET log

To ensure the technical functionality of your system, please review the
the PET log after *every simulation*. You should look out for entries in the
`ERROR` category anywhere and for input/output informational messages at the
beginning of the simulation to ensure that all your information is transported
correctly into and out of the coupleds system.

## How to get a log

By default (and recommendation), each processor writes full information in
its own PET log file, which can quickly accumulat to multiple Gb per PET and
constitute a severe limit on ***performance*** and on ***storage limitation***.

The recommended practice is to set the following flags in your `mossco_run.nml`
for production runs (you can even decrease the loglevel to 'none')

        loglevel = 'error',
        loglevelzero = 'trace',
        logflush = .false.,

This will give you sufficient output on PET 0 to analyse the performance, as well.
With  `logflush = .false.`, the log is placed in a buffer and only occasionally
written to disk.

For debugging, you can enable full and immediate output

        loglevel = 'all',
        loglevelzero = 'all',
        logflush = .true.,

## When to check your PET

Most of what can go wrong technically is apparent after the first timestep.  Thus, check your PET file quickly after your simulation started.

> Hack: you can start your simulation with `loglevel = 'all'` and quickly review
all PETs (or, do this for loglevelzero only); then you can delete/move the PET files and they will **not** be recreated, i.e. not waste storage space.

## Look for ERROR first

    grep -1 ERROR PET0.log

If an error occurs, this is cause for concern, please investigate and fix this, file a
bug report or contact us.

In some cases, an ERROR is produced from features that are only recently built into ESMF.
We provide a line 'ignore this error' to indicate those situations.  You can ignore these
kinds of error, or consider upgrading your ESMF installation.

## Analysing the log file for warnings

    grep -1 'WARNING'  PET0.log

Often, warnings are just that: warnings that things might have gone wrong.  Most often, you're fine.

## Analysing the log file for correct information transfer

This information is only visible at the `info` or `all` log level (recommended
`loglevelzero` only, or during debugging for all PET).


### Check input components

For every input component (e.g. boundaries, rivers, deposition, restarts), check that
the netCDF file is correctly read, that all variables correctly pass the include/exclude filters, and that an ESMF_Field is created.  

### Check nudge connector

Check that on those PET where it is relevant (e.g., those at the edge when boundary conditions
are used), cells are changed.

    grep cells PET0.log

### Check transport connector

    grep -1 'will transport' PET0.log
