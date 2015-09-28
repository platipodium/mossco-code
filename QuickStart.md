# Quickstart Intro

These instructions should get you started on building your own coupled system with MOSSCO. It is really only a quick start, if you or someone else has already installed

- Python with YAML support (python >= 2.7)
- Fortran2003 compliant compiler (e.g. PGI > 13.1, Intel > 12.0, GCC >= 4.8.2)
- CMake (>= 2.8.6)
- ESMF (The Earth System Modeling Framework) with NetCDF and MPI support (>= 6.3.0)

for you to use at this point. If not, you have to do it now (ESMF cannot be installed from the package manager). You will find some help at [www.mossco.de/doc](http://www.mossco.de/doc/index.html#installing-esmf).

Package                | Ubuntu        | Suse | MacPorts      | Fink
-----------------------|:--------------|:-----|:--------------|:---------------
compilers              | gcc, gfortran, g++ | gcc-4_8-branch revision 202388  | gcc48         | gcc48-compiler
NetCDF                 | libnetcdf-dev, netcdf-bin | libnetcdf-dev    |               | netcdf-c7, netcdf-fortran4, netcdf-cxx4
MPI                    | libmpich2-dev, mpich2     | openmpi-dev      | mpich-gcc48 *or* openmpi-gcc48   | openmpi
YAML                   | python-yaml   |Python-yaml | py27-yaml  | yaml-py27
cmake                  | cmake         | cmake      | cmake      | cmake

The ESMF installation finally provides a file `esmf.mk`, this location must be provided to MOSSCO, e.g.:

	export ESMFMKFILE=<path_to_esmf_install>/lib/lib0/Linux.gfortran.64.mpich2.default/esmf.mk

Depending on your shell, the above definition of environment variables could also follow the `csh` syntax

	setenv ESMFMKFILE <path_to_esmf_install>/lib/lib0/Linux.gfortran.64.mpich2.default/esmf.mk

Throughout this document, however, we will show the `bash` style commands only; you may freely substitute these with equivalent `csh` statements.

We will build a system that connects a pelagic ecosystem, running in a 1D ocean model to a benthic-pelagic ecosystem, using two external models

- FABM (Framework for Aquatic Biogeochemistry)
- GOTM (General Ocean Turbulence Model).

For these models, we will combine several MOSSCO components (you can find them in src/components)

- constant (Delivers constants to other components)
- gotm     (Hydrology using GOTM)
- fabm_pelagic (Ecology of FABM running in ocean)
- fabm_sediment (Ecology of FABM running in ocean soil)

# Prepare the environment for MOSSCO

First, define the directories where MOSSCO should be located on your system, and get the codes:

	export MOSSCO_DIR=$HOME/MOSSCO/code
	export MOSSCO_SETUPDIR=$HOME/MOSSCO/setups

	git clone git://git.code.sf.net/p/mossco/code $MOSSCO_DIR
	git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR

For those not already working with their own FABM and GOTM codes, the easiest way to obtain and use them within MOSSCO is:

    make -C $MOSSCO_DIR external

This will create corresponding source code directories in `$MOSSCO_DIR/external`, that will be recognised automatically by MOSSCO's `make` system.

Next, define the NETCDF environment variable needed for GOTM:

	export NETCDF=NETCDF4    # for GOTM

# Linking the mossco executable

MOSSCO provides a startup script located in `$MOSSCO_DIR/scripts/mossco.sh`.  We recommend that you create a link to this script somewhere in your `$PATH`, e.g.

	mkdir -p $HOME/opt/bin
	export PATH=$PATH:$HOME/opt/bin
	ln -sf $MOSSCO_DIR/scripts/mossco.sh $HOME/opt/bin/mossco

Type

	mossco -h

to learn about the options to this script.

# Compiling/running an example

To perform a simulation, you have to change to a directory containing all necessary setup files and execute `mossco` with the name of one of the provided examples, e.g.

	cd $MOSSCO_SETUPDIR/helgoland
	mossco gotm--fabm_pelagic--fabm_sediment--netcdf

You will see some screen output, and at least two files will be written

- `netcdf.nc` (a netcdf file containing all the output)
- `PET0.Helgoland` (a log file for your simulation)

# Going further

You can see many namelists here, of which the most important ones are

- `mossco_run.nml` (for controlling the simulation time)
- `fabm.nml` (for controlling the pelagic fabm models to be used)
- `run_sed.nml` (for controlling the benthic fabm models)
- `gotmrun.nml` (for controlling the hydrodynamic model)

Depending on your choice of FABM models, you may have to edit or add more namelist files, then execute `mossco gotm--fabm_pelagic--fabm_sediment--netcdf` again.

You can also explore this coupling configuration, which is specified in the file `$MOSSCO_DIR/examples/generic/gotm--fabm_pelagic--fabm_sediment--netcdf.yaml`.  After changing this coupling specification, issue

	mossco -r gotm--fabm_pelagic--fabm_sediment--netcdf

To rebuild and execute your example again.




