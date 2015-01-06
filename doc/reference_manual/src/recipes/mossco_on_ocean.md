# MOSSCO on ocean

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

## Obtaining external sources

	cd $MOSSCO_DIR
	make external # to obtain getm/gotm/fabm
	
## Compiling MOSSCO

	cd $MOSSCO_DIR
	make
	
