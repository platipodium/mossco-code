# Recipe #06: MOSSCO on ifksrv

Ifksrv are several local compute node at HZG. They offer recent Intel,  PGI, and GNU compilers and parallelism via openmpi.

This example assumes that you have downloaded or `git clone`d MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`, and have `git clone`d the MOSSCO setups into `$MOSSCO_SETUPDIR`.

    export MOSSCO_DIR=/my/path/to/mossco/code
    export MOSSCO_SETUPDIR=/my/path/to/mossco/setups

## Preparing your environment

    export COMPILER=gnu
    export PATH=/opt/${COMPILER}/openmpi/bin:/opt/${COMPILER}/netcdf/bin:${PATH}
    export PATH=$PATH:$HOME/opt/bin # for mossco.sh link
    export NETCDF=NETCDF4
    export NETCDF_VERSION=$NETCDF

### Specifics for the GNU compilers

ESMF is preinstalled in the following versions (choose one)

    export ESMFMKFILE=/ocean-home/lemmen/software/esmf-ifksrv/lib/libg/Linux.gfortran.64.openmpi.ESMF_7_1_0r/esmf.mk
    export ESMFMKFILE=/ocean-home/lemmen/software/esmf-ifksrv/lib/libO/Linux.gfortran.64.openmpi.ESMF_7_1_0r/esmf.mk
    
### Specifics for the Intel compilers

(only on ifksrv04, outdated ESMF)

    export ESMFMKFILE=/opt/intel/esmf_6_3_0rp1/lib/libO/Linux.intel.64.openmpi.default/esmf.mk
    source /opt/intel/bin/iccvars.sh intel64
    source /opt/intel/bin/ifortvars.sh intel64

### Specifics for the PGI compilers

(only on ifksrv04, outdated ESMF)

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
