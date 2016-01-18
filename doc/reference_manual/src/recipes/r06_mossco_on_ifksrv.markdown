# Recipe #06: MOSSCO on ifksrv

Ifksrv is a new local compute node at HZG. It offers recent Intel,  PGI, and GNU compilers and parallelism via openmpi.  ESMF is preinstalled with some of these compilers in subdirectories of

    /opt/gnu
    /opt/intel
    /opt/pgi

Choose a version that has both an `esmf.mk` and a `esmf.mod` file.

This example assumes that you have downloaded or `git clone`d MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`, and have `git clone`d the MOSSCO setups into `$MOSSCO_SETUPDIR`.

    export MOSSCO_DIR=/my/path/to/mossco/code
    export MOSSCO_SETUPDIR=/my/path/to/mossco/setups

## Preparing your environment

    export COMPILER=intel  # choose from gnu intel, or pgi
    export PATH=/opt/${COMPILER}/openmpi/bin:/opt/${COMPILER}/netcdf/bin:${PATH}
    export PATH=$PATH:$HOME/opt/bin # for mossco.sh link
    export NETCDF=NETCDF4
    export NETCDF_VERSION=$NETCDF

### Specifics for the GNU compilers

    export ESMFMKFILE=/opt/gnu/esmf_6_3_0rp1/lib/libO/Linux.gfortran.64.openmpi.default/esmf.mk

### Specifics for the Intel compilers

    export ESMFMKFILE=/opt/intel/esmf_6_3_0rp1/lib/libO/Linux.intel.64.openmpi.default/esmf.mk
    source /opt/intel/bin/iccvars.sh intel64
    source /opt/intel/bin/ifortvars.sh intel64

### Specifics for the PGI compilers

    export ESMFMKFILE=/opt/pgi/esmf_6_3_0rp1/lib/libO/Linux.pgi.64.openmpi.default/esmf.mk
    export PATH=/opt/pgi/linux86-64/2015/bin/:$PATH
    export MOSSCO_FFLAGS="-DNO_ISO_FORTRAN_ENV"

## Downloading MOSSCO and obtaining external sources

    mkdir -p $MOSSCO_DIR
    git clone git://git.code.sf.net/p/mossco/code $MOSSCO_DIR
    make -C $MOSSCO_DIR external # to obtain getm/gotm/fabm

.. and the same for the setups

    mkdir -p $MOSSCO_SETUPDIR
    git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR
