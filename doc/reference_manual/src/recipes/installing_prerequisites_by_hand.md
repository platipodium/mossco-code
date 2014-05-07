# Recipe 3: install your own software prerequisites

This document describes how to install all the prerequisites for running ESMF and MOSSCO not
with a package manager, but by a *manual install* of all required software packages.

All of the software required is available as open source software, please search for the source
code, often distributed as zipped archives on the internet and download the respective package.

## Assumptions

The instructions below assume, that you have already present in your system the facilities to `configure` and `make` software, i.e., that a recent enough version of `automake` and `autoconf` and a system C compiler is
already present.

## Installation directories

These instructions install all the software in the directory `PREFIX=$HOME/opt`, i.e., the libraries in `$PREFIX/lib`, header files in `$PREFIX/include`, and binaries in `$PREFIX/bin`.

To use the software, please add the binary directory to your search path and the library directory to your library search path

	export PATH=$PREFIX/bin:$PATH
	export LD_LIBRARY_PATH=$PREFIX/lib:$LD_LIBRARY_PATH

## Installing the new GNU Compiler Collection

Because of insufficient support for the FORTRAN 2003 standard, and because of bugs in the 4.7.x series
of GCC, MOSSCO requires GCC from version 4.8.0 onwards.  You can install this compiler yourself from
source.

GCC itself requires three software packages GMP, MPFR, and MPC to be installed (in this order).  So first go to the source directory for GMP and issue

	./configure --prefix=$PREFIX && make -j8 && make check && make install

Next, do the same in the MPFR directory, and point MPFR to your installation of GMP

	./configure --prefix=$PREFIX --with-gmp=$PREFIX && make -j8 && make check && make install

Next, do the same in the MPC directory, and point to your installations of GMP and mpfr

	./configure --prefix=$PREFIX --with-gmp=$PREFIX --with-mpfr=$PREFIX && make -j8 && make check && make install

Finally, go to the source directory of gcc and install the compiler.  Make soure that your `LD_LIBRARY_PATH` (see above) is defined.

	./configure --prefix=$PREFIX --with-gmp=$PREFIX --with-mpfr=$PREFIX --with-mpc=$PREFIX
	make -j8 && make check && make install

Compiling the compiler takes a long time (go do something else for half an hour to several hours).  But once
you are done, you can use your new shiny gcc compiler (and also gfortran and other languages) from `$PREFIX/bin`

## Installing OpenMPI

Change to your OpenMPI source directory, and issue the usual

	./configure --prefix=$PREFIX && make -j8 && make check && make install

## Installing netCDF

The current netCDF comes in three packages, one for C, one for Fortran and one for C++.  As a further complication, ESMF cannot be installed with the recent C++ API (also called C++4), but needs the *legacy* version of the netcdf C++ library.  As a requirment for new features of the netCDF-4 standard, the HDF-5 library is required and must be installed first.

So download HDF5, unzip, and change to the source directory

	./configure --prefix=$PREFIX && make -j8 && make check && make install

Then install the netcdf C library first

	./configure --prefix=$PREFIX && make -j8 && make check && make install

and later the fortran, C++4, and C++ legacy libraries. For each of these, issue

	./configure --prefix=$PREFIX && make -j8 && make check && make install







