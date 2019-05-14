# Recipe #28: MOSSCO on JUWELS

JUWELS (Jülich Wizard for European Leadership Science) is one of the current supercomputers at FZ Jülich (installed January 2018).  It has 2271 standard compute nodes (Dual Intel Xeon Platinum 8168)
with each node featuring 2x24 cores at 2.7 GHz and  12x8 Gb memory.  Other nodes support
large memory and GPU, see https://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JUWELS/Configuration/Configuration_node.html for a full list.  It runs a CentOS 7 Linux.

## Preparing your environment

ESMF 7.1.0r is preinstalled on this system with four different toolchains, choose one of the
four below

1. `module load GCC/8.3.0  ParaStationMPI/5.2.2-1`
2. `module load Intel/2019.3.199-GCC-8.3.0  IntelMPI/2019.3.19`
3. `module load Intel/2019.3.199-GCC-8.3.0  ParaStationMPI/5.2.2-1`

> We usually use the third option Intel/ParaStation

Then load ESMF and further requirements

		module load ESMF CMake Doxygen
		module load basemap netcdf4-python NCO CVS

		export ESMFMKFILE=$EBROOTESMF/lib/esmf.mk

		export NETCDF=NETCDF4
		export NETCDF_VERSION=$NETCDF
		export FORTRAN_COMPILER=IFORT

Consider placing your code and setups onto the `$PROJECT` directory.  For this,
you need to activate this environment

		jutil env activate -p <project>

Then define (do not create!) your `$MOSSCO_DIR` and `$MOSSCO_SETUPDIR` paths

		export $MOSSCO_DIR=$PROJECT/some/where/code        # just as an example
		export $MOSSCO_SETUPDIR=$PROJECT/some/where/setups # just as an example

Create the parent folder and clone MOSSCO code and setups into their folders

		mkdir -p $MOSSCO_DIR/.. $MOSSCO_SETUPDIR/..
		git clone git://git.code.sf.net/p/mossco/code $MOSSCO_DIR
		git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR

> Be sure to use your sf.net login if you want to be able to push.  Or, use
> the github repository, clone it and issue a pull request to make your
> contribution available upstream.

## Obtaining external sources and compiling MOSSCO

		make -C ${MOSSCO_SETUPDIR} external
		make -C ${MOSSCO_DIR} external

### Compiling MOSSCO and external codes

		make -C ${MOSSCO_DIR} all

### Installing the library and executables

		export MOSSCO_INSTALL_PREFIX=$HOME/opt # or any other location
		export PATH=PATH:$MOSSCO_INSTALL_PREFIX/bin
		export LD_LIBRARY_PATH=LD_LIBRARY_PATH:$MOSSCO_INSTALL_PREFIX/lib

		make -C ${MOSSCO_DIR} install

### Completing your environment

		export MOSSCO_USER_EMAIL=your@email.com  # for slurm notifications
		export MOSSCO_FFLAGS="-O3 -D_NEW_DAF_ -D_TEST_TRACERFLUXES_ -D_NO_SEALEVEL_CHECK_ -DMPIVERSION=2"
