# Recipe #10: MOSSCO on juropa

Juropa is the supercomputer at FZ Jülich.  You can apply for a testaccount by contacting their support at <dispatch@fz-juelich.de>.

Juropa offers ... everything

## Preparing your environment

Load the appropriate modules (Intel 11 is the default, we need a higher version)

	module purge
	module load intel/13.1.3 parastation/mpi-intel13-5.1.0
	module load netcdf autotools cmake git/1.8.3

    export ESMFMKFILE=/home/lemmen/opt/lib/libg/Linux.intel.64.openmpi.ESMF_7_0_0_beta_snapshot_04/esmf.mk

    export NETCDF=NETCDF4
    export NETCDF_VERSION=$NETCDF
    #export NETCDFINC=$(nc-config --includedir)

This example also assumes that you have downloaded or `git clone`ed MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`

## Obtaining external sources and compiling MOSSCO

    cd ${MOSSCO_DIR}
    make external # to obtain getm/gotm/fabm
    make


