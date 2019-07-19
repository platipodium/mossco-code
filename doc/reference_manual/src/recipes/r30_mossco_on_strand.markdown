# Recipe #30: MOSSCO on STRAND

STRAND is the local cluster at HZG. It offers recent Intel and GNU compilers and parallelism via openmpi and intelmpi.  STRAND is currently being customized for users, expect this document to change frequently.  This page gives instructions for building **MOSSCO**, **ESMF**, and **SCHISM** on STRAND.

## Preparing your environment

```bash
module load compilers/intel
module load intelmpi
module load netcdf

alias cmake='cmake3 '
```

> There is also a gcc/openmpi toolchain available

```bash
export MOSSCO_DIR=/your/path/to/mossco/code
export MOSSCO_SETUPDIR=/your/path/to/mossco/setups/sns
```

## Download and compilation of software

### ESMF

Try to use the preinstalled ESMF with

```bash
export ESMFMKFILE=/gpfs/home/lemmen/opt/lib/libO/Linux.intel.64.intelmpi.ESMF_7_1_0r/esmf.mk
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ESMFMKFILE%%/esmf.mk}
```

and continue to the next section.   Otherwise, it is easy to install ESMF yourself by setting the environment variables below, issueing `make && make install` and setting the `$ESMFMFKILE`

```bash
export ESMF_DIR=/your/path/to/esmf/code
export ESMF_INSTALL_PREFIX=/where/you/want/esmf/installed

git clone git://git.code.sf.net/p/esmf/esmf $ESMF_DIR

export ESMF_BOPT=O
export ESMF_ABI=64
export ESMF_MOAB=OFF
export ESMF_OPTLEVEL=2
export ESMF_LAPACK=internal
export ESMF_NETCDF=split
export ESMF_NETCDF_INCLUDE=`nf-config --includedir`
export ESMF_NETCDF_LIBPATH="/project/opt/software/netcdf/4.7.0/intel/lib -L/project/opt/software/hdf5/1.10.5/intel/lib"
export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE
unset ESMF_PIO
export ESMF_SITE=ESMF_7_1_0r
export ESMF_COMPILER=intel
export ESMF_COMM=intelmpi
unset ESMF_XERCES

cd ${ESMF_DIR}
make && make install
```

To use ESMF, add the following to your default environment:

```bash
export ESMFMKFILE=${ESMF_INSTALL_PREFIX}/lib/lib${BOPT}/Linux.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}/esmf.mk
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ESMFMKFILE%%/esmf.mk}
```

### MOSSCO

```bash
git clone git://git.code.sf.net/p/mossco/code $MOSSCO_DIR
git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR

make -C ${MOSSCO_SETUPDIR} external
make -C ${MOSSCO_DIR} external
make -C ${MOSSCO_DIR} all
```

To install libraries and executables (this is not mandatory), continue as follows

```bash
export MOSSCO_INSTALL_PREFIX=$HOME/opt # or any other location
export PATH=PATH:${MOSSCO_INSTALL_PREFIX}/bin
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${MOSSCO_INSTALL_PREFIX}/lib

make -C ${MOSSCO_DIR} install
```

### SCHISM

The installation of SCHISM is independent of MOSSCO, but both can be coupled together using ESMF and an installed MOSSCO library

```bash
export SCHISM_BUILD_DIR=/your/path/to/schism/build
export SCHISM_DIR=/your/path/to/schism/src
export PARMETIS_DIR=$SCHISM_DIR/ParMetis-3.1-Sep2010

svn checkout http://columbia.vims.edu/schism/trunk ${SCHISM_DIR}
(cd $PARMETIS_DIR; make)

mkdir -p $SCHISM_BUILD_DIR
cd $SCHISM_BUILD_DIR
cmake $SCHISM_SRC_DIR -DCMAKE_Fortran_COMPILER=mpiifort \
  -DCMAKE_CXX_COMPILER=mpicc -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DDNetCDF_FORTRAN_DIR=`nf-config --prefix` \
  -DNetCDF_C_DIR=`nc-config --prefix`\
  -DTVD_LIM=SB -DCMAKE_Fortran_FLAGS="-xCORE-AVX2"

make pschism
cp bin/pschism bin/pschism_hydro
```

You can add an independent FABM installation to SCHISM

```bash
export FABM_DIR=/your/path/to/fabm
git clone https://coastgit.hzg.de/hofmeist/fabm.git $FABM_DIR # HZG internal only, otherwise get public fabm repo

cd $SCHISM_BUILD_DIR
cmake $SCHISM_SRC_DIR  -DCMAKE_Fortran_COMPILER=mpiifort   \
  -DCMAKE_CXX_COMPILER=mpiicc -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DNetCDF_FORTRAN_DIR=`nc-config --prefix` \
  -DNetCDF_C_DIR=`nc-config --prefix`  -DTVD_LIM=SB \
  -DCMAKE_Fortran_FLAGS="-xCORE-AVX2"\
  -DUSE_FABM=ON -DFABM_BASE=$FABM_DIR

make pschism
cp bin/pschism bin/pschism_fabm
```
