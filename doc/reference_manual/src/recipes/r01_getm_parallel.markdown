# Recipe #01: Running both GETM and MOSSCO/GETM

This recipe describes how to get a GETM test case running in parallel within MOSSCO.

## Prerequisites
- have MOSSCO downloaded in `$MOSSCO_DIR`
- have GETM downloaded in `$GETMDIR`
- have GOTM downloaded in `$GOTMDIR`

## GETM setups

GETM setups are available from a separate repository.  It is not required to define
the environment varialbe `GETM_SETUPDIR`, however, we do this for convenience here.

		export GETM_SETUPDIR=$HOME/devel/getm/setups # choose any location
		git clone git://git.code.sf.net/p/getm/getm-setups $GETM_SETUPDIR

GETM should be compiled *within* a test case (because arrays are then allocated
correctly at compile time)

Most setups are only serial, As a parallel setup, choose  `box_cartesian`

		cd $GETM_SETUPDIR/box_cartesian

## Configure your environment and compile MOSSCO/GETM and GOTM

Set the following environment variables (yours may vary with a different
compiler or mpi implementation):

		export FORTRAN_COMPILER=GFORTRAN # choose this for gfortran
		export MPI=MPICH2

		export FORTRAN_COMPILER=IFORT    # choose this for intel
		export MPI=INTELMPI

Compile GOTM (note that you might need to adjust the variable
`CMAKE_Fortran_COMPILER`)

		export GOTM_PREFIX=$HOME/devel/gotm/install # choose any location
		mkdir -p ${GOTM_PREFIX}/../{build,install}

		cd ${GOTM_PREFIX}/../build && cmake ${GOTMDIR} -DCMAKE_INSTALL_PREFIX=${GOTM_PREFIX} -DGOTM_USE_FABM=off -DCMAKE_Fortran_COMPILER=mpiifort && make && make install

Compile a MOSSCO/GETM example, e.g.

		make -C $MOSSCO_DIR/examples/esmf/getm

This will create an executable `$MOSSCO_DIR/examples/esmf/getm/getm` that can
be run *independent* of the setup.

## Adjust the setup and compile GETM

For parallel execution, change the `parallel` setting in your getm.inp file

		export NETCDF=NETCDF4
		export GETM_PARALLEL=true

		cd $GETM_SETUPDIR/box_cartesian
		sed -i s/parallel=.false./parallel=.true./ getm.inp

		make

For MOSSCO, also create a file `mossco_run.nml` with start and stop dates as
in `getm.inp`

## Run the simulation

Then, execute the example with four processors.

		mpirun -np 4 $MOSSCO_DIR/examples/esmf/getm/getm
		mpirun -np 4 ./bin/getm_prod_IFORT
