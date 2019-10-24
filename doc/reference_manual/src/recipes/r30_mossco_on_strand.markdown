# Recipe #30: MOSSCO on STRAND

STRAND is the local cluster at HZG. It offers recent Intel and GNU compilers and parallelism via openmpi and intelmpi.  STRAND is currently being customized for users, expect this document to change frequently.  This page gives instructions for building **MOSSCO** and **SCHISM** on STRAND in the intel/intelmpi toolchain.

## Preparing your environment

```bash
module load compilers/intel/2018.1.163
module load intelmpi
module load esmf
module load netcdf
module load hdf5

alias cmake='cmake3 '
```

```bash
export MOSSCO_DIR=/your/path/to/mossco/code
export MOSSCO_SETUPDIR=/your/path/to/mossco/setups/sns
```

## Download and compilation of software

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
