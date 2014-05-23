# Creating a POP component

This document describes efforts to bring the Parallel Ocean model POP into ESMF

## Getting POP to run on your system

For this exercise, we assume that the source of POP is available in the direcotry `$POP_DIR`

1. Edit/create a Makefile snippet in the directory `$POPDIR/input_templates`.  In this file, e.g. `linux_mpi.gnu` set the compilers and netcdf paths and netcdf library.  

2. Export the variable `ARCHDIR` to point to your Makefile snippet

    cd $POPDIR; export ARCHDIR=linux_mpi

3. Run the `setup_run_dir` executable in `$POPDIR` with the name of a run directory as argument

	./setup_run_dir test test
	
4. Run `make` in your test directory

	cd $POPDIR/test; make
	
	
	