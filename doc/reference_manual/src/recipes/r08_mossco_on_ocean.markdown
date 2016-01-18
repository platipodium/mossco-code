# Recipe #08: MOSSCO on ocean

Ocean is the local cluster at HZG. It offers recent Intel and PGI compilers and parallelism via openmpi/ib (Infiniband).  ESMF is preinstalled with some of these compilers in subdirectories of

    /home/lemmen/opt

Choose a version that has both an `esmf.mk` and a `esmf.mod` file, e.g., the version `/home/lemmen/opt/lib/libg/Linux.intel.64.openmpi.ESMF_7_0_0_beta_snapshot_43/esmf.mk`


## Preparing your environment


### PyYAML

Download the latest PyYAML source package, then install it as a user

	
	python setup.py install --user </path/to/PyYAML/source/>
	
### Environment variables

    export PATH=$PATH:/home/lemmen/opt/bin # for cmake
    export ESMFMKFILE=/home/lemmen/opt/lib/libg/Linux.intel.64.openmpi.ESMF_7_0_0_beta_snapshot_43/esmf.mk
    module load intel openmpi_ib  netcdf/3.6.2

    export NETCDF=NETCDF3
    export NETCDF_VERSION=$NETCDF
    export NETCDFINC=$NETCDFHOME/include
    export NETCDFLIBDIR=$NETCDFHOME/lib

> Don't worry about `$NETCDFHOME`, it was defined by the call to `module load netcdf`.

This example also assumes that you have downloaded or `git clone`d MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`, and have `git clone`d the MOSSCO setups into `$MOSSCO_SETUPDIR`.

	export MOSSCO_DIR=/my/path/to/mossco/code
	export MOSSCO_SETUPDIR=/my/path/to/mossco/code
	
It is advisable to have `$MOSSCO_DIR` somewhere in your `/home` and `$MOSSCO_SETUPDIR` in your `/data` directory on ocean because of space restrictions (but not backup).

## Downloading MOSSCO and obtaining external sources

	mkdir -p $MOSSCO_DIR
	git clone git://git.code.sf.net/p/mossco/code $MOSSCO_DIR
    make -C $MOSSCO_DIR external # to obtain getm/gotm/fabm
	
.. and the same for the setups

	mkdir -p $MOSSCO_SETUPDIR
	git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR
    make -C $MOSSCO_SETUPDIR external # to obtain editscenario

> if you want to enable debugging, you can set `$MOSSCO_FFLAGS`

	export MOSSCO_FFLAGS="-g -C -check -traceback -check noarg_temp_created"

## Install ESMF on ocean

It is already done (usually), but in case you want to do this again:

		export ESMF_DIR=$HOME/ESMF/esmf-code
		mkdir -p $ESMF_DIR
                git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf $ESMF_DIR
		export ESMF_BOPT=g
		export ESMF_ABI=64
		export ESMF_MOAB=OFF
		export ESMF_OPTLEVEL=2
		export ESMF_INSTALL_PREFIX=$ESMF_DIR
		export ESMF_LAPACK=internal
		export ESMF_NETCDF=standard
		export ESMF_NETCDF_INCLUDE=/opt/netcdf/3.6.2/intel/include
		export ESMF_NETCDF_LIBPATH=/opt/netcdf/3.6.2/intel/lib
		export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE
		unset ESMF_PIO
		export ESMF_SITE=ESMF_7_0_0_beta_snapshot_43
		export ESMF_COMPILER=intel
		export ESMF_COMM=openmpi
		unset ESMF_XERCES
		
		cd $ESMF_DIR
		make lib

