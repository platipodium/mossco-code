# Recipe #2: GETM parallel in MOSSCO on ocean-fe (HZG)

This recipe describes the the run of a parallel GETM test case from $MOSSCO_DIR/examples/generic

## Prerequisites
- have MOSSCO downloaded in $MOSSCO_DIR
- have GETM downloaded in $GETMDIR
- have GOTM downloaded in $GOTMDIR

## GETM setupsyy
GETM setups are available from a separate repository.  It is not required to define the environment varialbe GETM_SETUPDIR, however, we do this for convenience here.

.  git clone git://git.code.sf.net/p/getm/getm-setups
.  export GETM_SETUPDIR=`pwd`/getm-setups

## Configure your environment and compile

Set the following environment variables (yours may vary with a different compiler or mpi implementation):

.  export NETCDF=NETCDF4
.  export GETM_PARALLEL=true
.  export FORTRAN_COMPILER=GFORTRAN
.  export MPI=MPICH2

Compile GOTM to get the turbulence library

.  make -C $GOTMDIR/src

Most setups are only serial, As a parallel setup, choose  `box_cartesian`
Normally, GETM should be compiled *within* a test case (because arrays are then allocated correctly at compile time).
For static compilation the file $GETMDIR/include/dimensions.h is required, because the compilation is done in this case with the makefile in $MOSSCO_DIR/examples/generic.
In the directory $GETM_SETUPDIR/box_cartesian configuration files for different processor counts are available, e.g. box_cartesian.4p.dim. Set a link to specify the desired configuration and adapt NPROCS in the testscript if not using 4 processes.
	ln -s $GETM_SETUPDIR/box_cartesian/box_cartesian.4p.dim $GETMDIR/include/dimensions.h

## Adjust the setup

For parallel execution, change the `parallel` setting in your getm.inp file
.  
.  cd $GETM_SETUPDIR/box_cartesian
.  sed -i s/parallel=.false./parallel=.true./ getm.inp

## Run the simulation
Now, execute the script test_getm_parallel_yaml.sh in the directory $MOSSCO_DIR/examples/generic.
	cd $MOSSCO_DIR/examples/generic
	./test_getm_parallel_yaml.sh

The script uses the file getm_coupling.yaml to generate a toplevel component for a parallel getm run, set up a batch jobscript and execute it on ocean.
