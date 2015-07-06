# Recipe #11: MOSSCO on juropatest

Juropatest is the test environment for the next supercomputer at FZ Jülich.  You can apply for a testaccount by contacting their support at <dispatch.jsc@fz-juelich.de>.

## System description

See <http://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JUROPATEST/Configuration/Configuration_node.html> for the configuration.  The batch system currently allows the use of 16 nodes with 2 CPU x 14 core processors, i.e. a total of 448 cores.

## Preparing your environment

Load the appropriate modules

		module purge

		module load intel-para/2015.06-mt
		module load CMake Doxygen
		module load netCDF-Fortran
		module load netCDF4-python PyYAML matplotlib
		module load Xerces-C++
		module load NCO

    export ESMFMKFILE=$HOME/opt/esmf/lib/libg/Linux.intel.64.mpich2.default/esmf.mk

    export NETCDF=NETCDF4
    export NETCDF_VERSION=$NETCDF
    export FORTRAN_COMPILER=IFORT

This example also assumes that you have downloaded or `git clone`ed MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`

## Obtaining external sources and compiling MOSSCO

    cd ${MOSSCO_DIR}
    make external # to obtain getm/gotm/fabm
    make


## Installing ESMF (if not done so already)

You should not have to do this, it is documented here for administrators.

	export ESMF_DIR=${HOME}/devel/ESMF/code
    export ESMF_BOPT=g
	export ESMF_OPTLEVEL=2
	export ESMF_COMM=mpich2
	export ESMF_NETCDF=split
    export ESMF_PNETCDF=standard
	export ESMF_INSTALL_PREFIX=${HOME}/opt/esmf
	export ESMF_COMPILER=intel
	export ESMF_OS=Linux

	export ESMF_NETCDF_INCLUDE=$(nc-config --includedir)
	export ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib
	export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE

	(cd $ESMF_DIR; make esmflib && make install)
