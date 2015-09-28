# Recipe #9: Using mossco.sh startup script

We have created a convencience/wrapper script `mossco.sh` to make it easier for you to run mossco setups and examples.

## Preparing your environment

For this recipe, we assume that you have defined the two environment variables `$MOSSCO_DIR`  and `$MOSSCO_SETUPDIR`

First of all, link the script to a convenient location in your local `PATH`, for example by issuing

		ln -s ${MOSSCO_DIR}/scripts/mossco.sh ${HOME}/opt/bin/mossco

If not already done by your system, add this local binary path to your `$PATH` variable

		export PATH=${PATH}:${HOME}/opt/bin

## Compiling and running an example

The `mossco.sh` script facilitates compiling and running MOSSCO example executables (both hardcoded coupled system and on-the-fly generated new coupled systems) in any directory that contains setup information.

For example, change to the 3d deep lake setup

		cd ${MOSSCO_SETUPDIR}/deep_lake


In this setup, execute the `mossco.sh` script (linked to by `mossco`) with one argument that denotes the name of the example you would like to execute.  The default argument is `benthic_geoecology`, which is one of our 1D setups.  List the directory

		ls ${MOSSCO_DIR}/examples

to find names of hardcoded coupled system examples that you could specify.

The `deep_lake` setup contains suitable information of 3D examples, such as `getm--fabm_pelagic--netcdf`, so we will run this

		mossco getm--fabm_pelagic--netcdf

This will take care of all compilation necessary and of executing the script in this setup.

The generation of coupled systems relies on a coupling specification in a `.yaml` format.  You can find several of those configuration specifications in `${MOSSCO_DIR}/examples/generic`. If your personal yaml file (not necessarily located in ${MOSSCO_DIR}/examples/generic) cannot be found, an existing homonymous generic example or an existing homonymous hardcoded examples will be taken.


## Options to mossco.sh

### -r: recompile

After changing the code in `${MOSSCO_DIR}`, you might want to recompile before executing, then use the `-r` option

		mossco -r getm--fabm_pelagic--netcdf

