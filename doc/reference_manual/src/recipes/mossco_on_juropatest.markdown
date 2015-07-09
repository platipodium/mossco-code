# Recipe #11: MOSSCO on juropatest

Juropatest is the test environment for the next supercomputer "Jureca" at FZ Jülich.  
You can apply for a test account by contacting their support at <dispatch.jsc@fz-juelich.de>.

## System description

See <http://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JUROPATEST/Configuration/Configuration_node.html>
for the configuration.  The batch system currently allows the use of 16 nodes with 2 CPU x 14 core processors, i.e. a total of 448 cores.

## Preparing your environment

### Load the appropriate modules

		module purge
		module load intel-para/2015.06-mt
		module load CMake Doxygen
		module load netCDF-Fortran
		module load netCDF4-python PyYAML matplotlib
		module load Xerces-C++
		module load NCO

### Set environment variables

export NETCDF=NETCDF4
export NETCDF_VERSION=$NETCDF
export FORTRAN_COMPILER=IFORT
export MOSSCO_SETUPDIR=/your/path/to/mossco/setups
export MOSSCO_DIR=/your/path/to/mossco/code

### Set the ESMF Makefile location

		export ESMFMKFILE=/your/path/to/esmf.mk

> If ESMF is not installed, please see the section below on ESMF installation

## Obtaining MOSSCO and external sources

		git clone git://git.code.sf.net/p/mossco/code ${MOSSCO_DIR}
		make -C ${MOSSCO_DIR} external # to obtain getm/gotm/fabm

## "Installing" MOSSCO startup script

		mkdir -p ${HOME}/opt/bin
		export PATH=${PATH}:${HOME}/opt/bin
		ln -sf ${MOSSCO_DIR}/src/scripts/mossco.sh ${HOME}/opt/bin/mossco

> These instructions are valid for a bash/dash shell, your commands may
> vary for a csh/ksh-like shell.  Of course, you can choose a different
> directory for installation.

## Running your first MOSSCO coupled system

		git clone git://git.code.sf.net/p/mossco/setups ${MOSSCO_SETUPDIR}
		cd ${MOSSCO_SETUPDIR}/deep_lake
		mossco -n12 gffrr

## Installing ESMF

if not done so already.  Preferably, ask your system administrator.

		export ESMF_DIR=/your/path/to/where/you/want/esmf
		git clone http://git.code.sf.net/p/esmf/esmf ${ESMF_DIR}
		cd ${ESMF_DIR}

		export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE
		export ESMF_COMPILER=intel
		export ESMF_COMM=mpich2
		export ESMF_LAPACK=mkl
		export ESMF_LAPACK_LIBS="-lmkl_lapack -lmkl"
		export ESMF_NETCDF=split
		unset ESMF_PNETCDF
		unset ESMF_PIO
		export ESMF_XERCES=standard
		export ESMF_BOPT=g
		export ESMF_OPTLEVEL=2
		export ESMF_INSTALL_PREFIX=${HOME}/opt

		(cd $ESMF_DIR; make lib && make install)

> The install command does not work so far, you would have to manually correct the
> ESMFMKFILE to point to the location of esmf.mk found within $ESMF_DIR/lib
