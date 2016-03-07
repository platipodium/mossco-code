# Recipe #19: creating your own grid setup

This document describes how to create a setup where you can use the sediment component based on
an arbitrary grid you specified by a SCRIP netcdf file.

## Locating files

Make your own directory `${WD}`and copy some of the template files provided by MOSSCO into this working
directory.

        export WD=${MOSSCO_SETUPDIR}/grid  # or name it differently
        mkdir -p ${WD} && cd ${WD}
        # Copy the coupling specification to change it later
        cp ${MOSSCO_DIR}/examples/generic/grid--fabm_sediment.yaml grid.yaml
        # Copy the grid creation script for later modification
        cp ${MOSSCO_DIR}/scripts/create_scrip.py .
        # Copy the fabm_sediment namelists and constant.dat file
        cp ${MOSSCO_SETUPDIR}/sns/constant.dat .
        cp ${MOSSCO_SETUPDIR}/sns/run_sed.nml .
        cp ${MOSSCO_SETUPDIR}/sns/fabm_sed.nml .
        # Copy some mossco run namelists
        cp ${MOSSCO_SETUPDIR}/sns/mossco_run.nml .

## First test with default configuration

Create a first SCRIP-compliant netCDF file with `create_scrip.py`:

        python create_scrip.py

This will produce a file called `scrip_example.nc` that contains the 2D grid definition. Now, tell
the grid component to read this file from its configuration file `grid.cfg`.  Also add the number
of layers (0 for 2D-files, 3D is not implemented yet.)

        echo "grid: scrip_example.nc" > grid.cfg
        echo "layers: 0" >> grid.cfg

Now compile and run mossco, e.g. with the command

        mossco grid

Check the resulting `PET` logs and netCDF files for errors and technical validity.  

## Make your own changes

Now proceed to

1. editing `create_scrip.py` to produce a different spatial layout
2. editing `grid.yaml` to substitute the coupling to fabm_sediment with your own component
