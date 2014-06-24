# Recipe #2: GETM parallel in MOSSCO on ocean-fe (HZG)

This recipe describes the the run of a parallel GETM test case from $MOSSCO_DIR/examples/generic

## Prerequisites
- have MOSSCO downloaded in $MOSSCO_DIR
- have GETM downloaded in $GETMDIR
- have GOTM downloaded in $GOTMDIR

## GETM setups
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

Most setups are only serial. As a parallel setup, choose  `box_cartesian`.
Normally, GETM should be compiled *within* a test case (because arrays are then allocated correctly at compile time).
For static compilation the file $GETMDIR/include/dimensions.h is required, because the compilation is done in this case with the makefile in $MOSSCO_DIR/examples/generic.
In the directory $GETM_SETUPDIR/box_cartesian configuration files for different processor counts are available, e.g. box_cartesian.4p.dim. You can set a link to specify the desired configuration and adapt NPROCS in the testscript if not using 4 processes.
	ln -s $GETM_SETUPDIR/box_cartesian/box_cartesian.4p.dim $GETMDIR/include/dimensions.h

## Adjust the setup

For parallel execution, the `parallel` setting in the getm.inp file should be changed.
.  
.  cd $GETM_SETUPDIR/box_cartesian
.  sed -i s/parallel=.false./parallel=.true./ getm.inp

## Run the simulation
The script test_getm_parallel_yaml.sh in the directory $MOSSCO_DIR/examples/generic does all settings mentioned above.
Edit the number of processes in both grid directions. For a two-dimensional mesh the values have to be greater than one.
Originally designed for 4 processes the values inside box_cartesion.4p.dim are adapted to the chosen mesh size, because mosscow should not change the defaults of the cartesion box example
	cd $MOSSCO_DIR/examples/generic
	./test_getm_parallel_yaml.sh

The script uses the file getm_coupling.yaml to generate a toplevel component for a parallel getm run, set up a batch jobscript and execute it on ocean.

The resulting netcdf files can be assembled together into a single netcdf file with the program ncmerge on ocean or the groupserver at HZG.
