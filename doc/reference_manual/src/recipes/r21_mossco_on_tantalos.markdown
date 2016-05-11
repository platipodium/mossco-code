# Recipe #09: MOSSCO on tantalos

tantalos is the local cluster at BAW. It offers recent Intel compilers, gcc 4.8.2 as module  and parallelism via mpi (mpt- from SGI and mpich).  

First gfortran and mpt modules should be loaded:

module load gcc/4.8.2
module load mpt/2.06


## Preparing your environment

### PyYAML

Download the latest PyYAML source package, then install it as a user

	python setup.py install --user </path/to/PyYAML/source/>

### Environment variables (for GNU compiler)
#### ESMF
It is assumed that netcdf3.6.3 has been already installed

export NETCDFHOME=$MOSSCO_BASE/07_Netcdf/netcdf_3.6.3
export ESMF_NETCDF_INCLUDE=$NETCDFHOME/include
export ESMF_NETCDF_LIBPATH=$NETCDFHOME/lib
export ESMF_NETCDF="standard"
export ESMF_NETCDF_LIBS="-lnetcdf"

export ESMF_COMM=mpi
export ESMF_COMPILER=gfortran
export ESMFMKFILE=$ESMF_DIR/lib/libO/Linux.gfortran.64.mpi.ESMF_7_1_0_beta_snapshot_00/esmf.mk
source $HOME/.esmf_Linux.gfortran.64.mpi.ESMF_7_1_0_beta_snapshot_00
unset ESMF_XERCES

#### GETM

export MPIINC="$HOME/include -I/sw/sdev/mpt-x86_64/2.06-p10901/include"

#### Python

export PYTHONPATH=/home/ak2mnase/lib64/python2.6/site-packages

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

## Installing Netcdf3.6.3
 gunzip netcdf-3.6.2.tar.gz
     tar -xf netcdf-3.6.2.tar
     cd netcdf-3.6.2
./configure --prefix=/net/themis/meer/nordsee/MOSSCO/03_Modelle/07_Netcdf/netcdf_3.6.3
make check
make install

An importat note is that not to install netcdf in the unzip folder but create a new folder and then use that folder for make check check and make install as shown above by --prefix

## Install ESMF on tantalos

It is already done, but in case you want to do this again:

		export ESMF_DIR=$HOME/ESMF/esmf-code
		mkdir -p $ESMF_DIR
                git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf $ESMF_DIR
                cd $ESMF_DIR
		make lib
