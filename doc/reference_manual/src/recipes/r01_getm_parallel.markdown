# Recipe #01: GETM parallel in MOSSCO

This recipe describes how to get a GETM test case running in parallel within MOSSCO. 

## Prerequisites
- have MOSSCO downloaded in $MOSSCO_DIR
- have GETM downloaded in $GETMDIR
- have GOTM downloaded in $GOTMDIR

## GETM setups

GETM setups are available from a separate repository.  It is not required to define the environment varialbe GETM_SETUPDIR, however, we do this for convenience here. 

	git clone git://git.code.sf.net/p/getm/getm-setups
	export GETM_SETUPDIR=`pwd`/getm-setups

GETM should be compiled *within* a test case (because arrays are then allocated correctly at compile time)

Most setups are only serial, As a parallel setup, choose  `box_cartesian`

	cd $GETM_SETUPDIR/box_cartesian

## Configure your environment and compile
	
Set the following environment variables (yours may vary with a different compiler or mpi implementation):

	export NETCDF=NETCDF4 
	export GETM_PARALLEL=true
	export FORTRAN_COMPILER=GFORTRAN
	export MPI=MPICH2
	
Compile GOTM to get the turbulence library

	make -C $GOTMDIR/src
	
Compile a GETM example, e.g. 

	make -C $MOSSCO_DIR/examples/esmf/getm

## Adjust the setup

For parallel execution, change the `parallel` setting in your getm.inp file 
	
	cd $GETM_SETUPDIR/box_cartesian
	sed -i s/parallel=.false./parallel=.true./ getm.inp

## Run the simulation
	
Then, execute the example with four processors.

	mpirun -np 4 $MOSSCO_DIR/examples/esmf/getm/getm
	
	

	



