# Recipe #30: MOSSCO on strand

Strand is the local cluster at HZG. It offers recent Intel and GNU compilers and parallelism via openmpi and intelmpi.  Strand is currently being customized for users, expect this
document to change frequently.

## Preparing your environment

    module load compilers/gnu/system_default
    module load netcdf  openmpi/2.1.2

> The default openmpi module is currently being tested as well.  The intel toolchain
> is currently broken in `netcdf`

### ESMF

You should be able to compile and install ESMF with the above toolchain.  For local
users, you can also use the preinstalled ESMF at

    export ESMFMKFILE=/home/lemmen/opt/lib/libO/Linux.intel.64.openmpi.ESMF_7_1_0r/esmf.mk
