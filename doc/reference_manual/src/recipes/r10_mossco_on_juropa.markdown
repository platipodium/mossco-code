# Recipe #10: MOSSCO on Juropa

Juropa is one of the courrent supercomputers at FZ Jülich. The name is short for "Jülich Research on Petaflop Architectures".  You can apply for a testaccount by contacting their support at <dispatch.jsc@fz-juelich.de>.


## Preparing your environment

Load the appropriate modules (Intel 11 is the default, we need a higher version)

	module purge
	module load intel/13.1.3 parastation/mpi-intel13-5.1.0
	module load netcdf autotools cmake git/1.8.3

    export ESMFMKFILE=${HOME}/opt/esmf/lib/libO/Linux.intel.64.mpich2.default/esmf.mk

    export NETCDF=NETCDF4
    export NETCDF_VERSION=$NETCDF
    #export NETCDFINC=$(nc-config --includedir)
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
	export ESMF_INSTALL_PREFIX=${HOME}/opt/esmf
	export ESMF_COMPILER=intel
	
	export ESMF_NETCDF_INCLUDE=$(nc-config --includedir)
	export ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib
	export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE

	(cd $ESMF_DIR; make && make install)