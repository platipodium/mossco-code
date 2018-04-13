# Quickstart Intro

These instructions should get you started on building your own coupled system with
MOSSCO. It is really only a quick start, if you or someone else has already installed

- Python with YAML support (python >= 2.7)
- Fortran2003 compliant compiler (e.g. PGI > 13.1, Intel > 12.0, GCC >= 4.8.3)
- CMake (>= 2.8.6)
- ESMF (The Earth System Modeling Framework) with NetCDF and MPI support (>= 7.1.0)

for you to use at this point. If not, you have to do it now.  For some port managers
we provide a list of packages you need.  On Ubuntu Linux, you would usally use `apt`,
on MacOS `ports` or `fink`.

| Package/Installer | apt                       | yast           | ports          | fink                       | conda |
| ----------------- |:------------------------- |:-------------- |:-------------- |:-------------------------- |:----- |
| compilers         | gfortran, g++             | gcc-4_8-branch | clang50        | gcc48-compiler             |       |
| NetCDF            | libnetcdf-dev, netcdf-bin | libnetcdf-dev  | netcdf-fortran | netcdf-c7, netcdf-fortran4 |       |
| MPI               | libmpich2-dev, mpich2     | openmpi-dev    | openmpi        | openmpi                    |       |
| YAML              | python-yaml               | Python-yaml    | py27-yaml      | yaml-py27                  |       |
| cmake             | cmake                     | cmake          | cmake          | cmake                      |       |
| ESMF              | n/a                       | n/a            | esmf           | n/a                        | esmf  |

If your package manager cannot install ESMF, you will find some help at
[www.mossco.de/doc](http://www.mossco.de/doc/index.html#installing-esmf).

The ESMF installation finally provides a file `esmf.mk`, this location must be
provided to MOSSCO.  On my Linux system with gfortran and mpich, the path is, e.g.:

		export ESMFMKFILE=<path_to_esmf_install>/lib/lib0/Linux.gfortran.64.mpich2.default/esmf.mk

Depending on your shell, the above definition of environment variables could also
follow the `csh` syntax

		setenv ESMFMKFILE <path_to_esmf_install>/lib/lib0/Linux.gfortran.64.mpich2.default/esmf.mk

Throughout this document, however, we will show the `bash` style commands only;
you may freely substitute these with equivalent `csh` statements.

We will build a system that connects a pelagic ecosystem, running in a 1D ocean
model to a benthic-pelagic ecosystem, using two external models

- FABM (Framework for Aquatic Biogeochemistry)
- GOTM (General Ocean Turbulence Model).

For these models, we will combine several MOSSCO components (you can find them in
`src/components`)

- `default` (Delivers constants to other components)
- `gotm`     (Hydrology using GOTM)
- `fabm_pelagic` (Ecology of FABM running in ocean)
- `fabm_sediment` (Ecology of FABM running in ocean soil)

# Prepare the environment for MOSSCO and download

First, define the directories where MOSSCO should be located on your system, and
get the codes:

		export MOSSCO_DIR=$HOME/MOSSCO/code
		export MOSSCO_SETUPDIR=$HOME/MOSSCO/setups

		git clone --depth=1 git://git.code.sf.net/p/mossco/code $MOSSCO_DIR
		git clone --depth=1 git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR

Alternatively, you can obtain a snapshot from https://sourceforge.net/projects/mossco/files/Snapshots/.

# Preparing external programs for use with MOSSCO

For those not already working with their own FABM and GOTM codes, the easiest way
to obtain and use them within MOSSCO is:

		make -C $MOSSCO_DIR external

This will create corresponding source code directories in `$MOSSCO_DIR/external`,
that will be recognized automatically by MOSSCO's `make` system.

Next, define the NETCDF environment variable needed for GOTM:

		export NETCDF=NETCDF4    # for GOTM

# Linking the mossco executable

MOSSCO provides a startup script located in `$MOSSCO_DIR/scripts/mossco.sh`.  We
recommend that you create a link to this script somewhere in your `$PATH`, e.g.

		mkdir -p $HOME/opt/bin
		export PATH=$PATH:$HOME/opt/bin
		ln -sf $MOSSCO_DIR/scripts/mossco.sh $HOME/opt/bin/mossco

Type

		mossco -h

to learn about the options to this script.

# Compiling/running an example

To perform a simulation, you have to change to a directory containing all
necessary setup files and execute `mossco` with the name of one of the provided
examples, e.g.

		cd $MOSSCO_SETUPDIR/helgoland
		mossco gotm--fabm_pelagic--fabm_sediment

> Most provided example couplings have an abbreviation, the above, e.g., can also
> be run with the command `mossco jfs`.  See the directory
> `$MOSSCO_DIR/examples/generic` for more examples.

You will see some screen output, and at least two files will be written

- `mossco_jfs.nc` (a netcdf file containing all the output)
- `PET0.helgoland-1x1-gotm--fabm_pelagic--fabm_sediment` (a log file for your simulation)

Investigate the results file with a netCDF viewer (e.g. Panoply, ncview,
ParaView, ncBrowse, ...), and have a look at the log file to see how the
coupling was documented.

# Changing the simple 1D setup

You can see many namelists here, of which the most important ones are

- `mossco_run.nml` (for controlling the simulation time)
- `fabm.nml` (for controlling the pelagic fabm models to be used)
- `run_sed.nml` (for controlling the benthic fabm models)
- `gotmrun.nml` (for controlling the hydrodynamic model)

Depending on your choice of FABM models, you may have to edit or add more namelist
files, then execute `mossco gotm--fabm_pelagic--fabm_sediment` again.

You can also explore this coupling configuration, which is specified in the file `$MOSSCO_DIR/examples/generic/gotm--fabm_pelagic--fabm_sediment.yaml`.
After changing this coupling specification, issue

		mossco -r gotm--fabm_pelagic--fabm_sediment

To rebuild and execute your example again.

# Not working?

Should you encounter any errors or should these instructions fail or are
insufficient we value your feedback in our bug tracker at <http://www.mossco.de/bugs>.

# Going further to 3D

If you downloaded via the `make external` command above, you will also have
obtained a copy of the General Estuarine Transport Model (GETM), a full
threedimensional hydrodynamic model.  To use this model, add

		export NETCDF_VERSION=NETCDF4   # for GETM

to your environment and change to the deep_lake example, where you can execute
a predefined 3D-coupled system like `gfs` in parallel (e.g. 4 processes) with
the `mossco` command

		cd $MOSSCO_SETUPDIR/deep_lake
		mossco -n 4 gfs

The results will be written to the files `mossco_gfs.X.nc`, where `X` denotes the
process number.  These files can be laterally merged with the stitch tool provided in `$MOSSCO_DIR/scripts/postprocess/stitch_tiles.py`. Install and run this python script:

		ln -sf $MOSSCO_DIR/scripts/postprocess/stitch_tiles.py $HOME/opt/bin/stitch
		stitch mossco_gfs.nc

To obtain the full 3D output in one file, that is likely named
 `mossco_gfs.nc_stitched.nc`.  Proceed by analysing this output with your
 preferred netCDF viewer.
