# Recipe #8: MOSSCO on ocean

Ocean is the local cluster at HZG. It offers recent Intel and PGI compilers and parallelism via openmpi/ib (Infiniband).  ESMF is preinstalled with some of these compilers in subdirectories of

    /home/lemmen/opt

Choose a version that has both an `esmf.mk` and a `esmf.mod` file, e.g., the version `/home/lemmen/opt/lib/libg/Linux.intel.64.openmpi.ESMF_7_0_0_beta_snapshot_04/esmf.mk`


## Preparing your environment

    export PATH=$PATH:/home/lemmen/bin # for cmake
    export ESMFMKFILE=/home/lemmen/opt/lib/libg/Linux.intel.64.openmpi.ESMF_7_0_0_beta_snapshot_04/esmf.mk
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

## Downloading MOSSCO

	mkdir -p $MOSSCO_DIR
	git clone git://git.code.sf.net/p/mossco/code $MOSSCO_DIR
	
	mkdir -p $MOSSCO_SETUPDIR
	git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR

## Obtaining external sources and compiling MOSSCO

    cd $MOSSCO_DIR
    make external # to obtain getm/gotm/fabm
    make


