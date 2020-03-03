# Recipe #30: MOSSCO and SCHISM on STRAND

STRAND is the local cluster at HZG. It offers recent Intel and GNU compilers and
parallelism via `openmpi` and `intelmpi`.

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

The installation of SCHISM is independent of MOSSCO, but both can be coupled
together using ESMF and an installed MOSSCO library

```bash
export SCHISM_BUILD_DIR=/your/path/to/schism/build
export SCHISM_DIR=/your/path/to/schism/src

git clone https://github.com/schism-dev/schism.git ${SCHISM_DIR}

mkdir -p $SCHISM_BUILD_DIR && cd $SCHISM_BUILD_DIR

cmake $SCHISM_SRC_DIR -DCMAKE_Fortran_COMPILER=mpiifort \
  -DCMAKE_CXX_COMPILER=mpicc -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DDNetCDF_FORTRAN_DIR=`nf-config --prefix` \
  -DNetCDF_C_DIR=`nc-config --prefix`\
  -DTVD_LIM=SB -DCMAKE_Fortran_FLAGS="-xCORE-AVX2"

make pschism
```

### SCHISM/FABM

You can add an independent FABM installation to SCHISM

```bash
export FABM_DIR=/your/path/to/fabm
git clone https://coastgit.hzg.de/gcoast/fabm.git $FABM_DIR # HZG internal only, otherwise get public fabm repo

cd $SCHISM_BUILD_DIR
cmake $SCHISM_SRC_DIR  -DCMAKE_Fortran_COMPILER=mpiifort   \
  -DCMAKE_CXX_COMPILER=mpiicc -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DNetCDF_FORTRAN_DIR=`nc-config --prefix` \
  -DNetCDF_C_DIR=`nc-config --prefix`  -DTVD_LIM=SB \
  -DCMAKE_Fortran_FLAGS="-xCORE-AVX2"\
  -DUSE_FABM=ON -DFABM_BASE=$FABM_DIR

make pschism
```

### SCHISM/ESMF

The ESMF interface to SCHISM is currently available in a separate repository.

```bash
export SCHISM_ESMF_DIR=/your/path/to/schism/esmf
git clone https://github.com/schism-dev/schism-esmf.git
```

## Batch script

Submit your job with `srun` if using the intelmpi toolchain.

    srun --mpi=pmi2 <your-exe> 
