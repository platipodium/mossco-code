# Recipe 3: install your own software prerequisites

This document describes how to install all the prerequisites for running ESMF and MOSSCO not
with a package manager, but by a *manual install* of all required software packages.

This script was tested on 

- Linux 2.6.39-400.214.3.el5uek SMP x86_64 24 Intel(R) Xeon(R) CPU E5645 @ 2.40GHz
- Linux 3.13.0-24-generic SMP x86_64 12 Intel(R) Xeon(R) CPU E5649  @ 2.53GHz
- Linux 2.6.16.60-0.37_f594963d_lustre.1.8.0.1-smp  SMP x86_64  4 Dual Core AMD Opteron(tm) Processor 285


## Assumptions

The instructions below assume, that you have already present in your system the facilities to `configure` and `make` software, i.e., that a recent enough version of `automake` and `autoconf` and a system C compiler is
already present.

## Installation directories

These instructions install all the software in the directory `PREFIX=$HOME/opt`, i.e., the libraries in `$PREFIX/lib`, header files in `$PREFIX/include`, and binaries in `$PREFIX/bin`.  

    export PREFIX=$HOME/opt

If you have root access on your system, we recommend to set `PREFIX=/opt/gcc49` or similar, as the installed software will be compiler-specific.  To avoid having to use `sudo` privileges in your installation, you should make sure that the directory `$PREFIX` is owned by your local user.  If not, ensure this with

    sudo chown -R your-user-name $PREFIX

To use the software, please add the binary directory to your search path and the library directory to your library search path

	export PATH=$PREFIX/bin:$PATH
	export LD_LIBRARY_PATH=$PREFIX/lib:$LD_LIBRARY_PATH
	
	
## Software versions
In this tutorial, we chose a working set of software versions; by no means we intend to restrict you to using these specific versions.  You would have to adust download URLs and paths accordingly, if you chose alternate versions.
	
In this example, we chose to download the software to the directory `WORK=$HOME/Downloads`, unpack it there, build it there, and later install it to `$PREFIX`.

   export WORK=$HOME/Downloads


## Installing the new GNU Compiler Collection

Because of insufficient support for the FORTRAN 2003 standard, and because of bugs in the 4.7.x series
of GCC, MOSSCO requires GCC from version 4.8.0 onwards.  You can install this compiler yourself from
source.

    cd $WORK
    wget http://download.heise.de/software/f9cfb08c2f4cf5210863561e8a29c168/5379ee64/120272/gcc-4.9.0.tar.gz
    tar xzf gcc-4.9.0.tar.gz
    cd gcc-4.9.0

GCC itself requires three software packages GMP, MPFR, and MPC to be installed (in this order).  You can do this manually, but there's 
a good chance to fail since the gcc `./configure` script is not smart enough yet.  Fortunately, gcc comes with a a script to take care 
of its dependencies [see this discussion](http://gcc.gnu.org/wiki/FAQ#configure).

In the GCC source directory, run

    ./contrib/download_prerequisites 
    
Create a directory next to the gcc source directory
    
    mkdir ../gcc-build ; cd ../gcc-build
    
From within this directory, run the `../gcc-4.9.0/configure` script and make

	../gcc-4.9.0/configure --prefix=$PREFIX
	make -j8 && make check && make install

Compiling the compiler takes a long time (go do something else for half an hour to several hours).  But once
you are done, you can use your new shiny gcc compiler (and also gfortran and other languages) from `$PREFIX/bin`

## Installing OpenMPI

    cd $WORK
    wget http://www.open-mpi.org/software/ompi/v1.8/downloads/openmpi-1.8.1.tar.gz
    tar xzf openmpi-1.8.1.tar.gz
    cd openmpi-1.8.1

Issue the usual

	./configure --prefix=$PREFIX && make -j8 && make check && make install

From now on, *do not* use gcc/gfortran/g++ as your compilers, but *use mpifort/mpicc/mpiCC* as your compilers.  You can tell this to your system
by setting
    
    export FC=mpifort
    export CC=mpicc
    export CXX=mpiCC
    
(or use equivalent `csh` syntax, e.g. `setenv FC mpifort`).  Note that with recent versions of OpenMPI, the commands `mpif90` and `mpif77` are deprecated.

## Installing netCDF

The current netCDF comes in three packages, one for C, one for Fortran (and one for C++, which is not needed in recent versions of ESMF).  As a requirement for new features of the netCDF-4 standard, the HDF-5 library is required and must be installed first.

To get all the software, issue the following:

    cd $WORK
    wget http://www.hdfgroup.org/ftp/HDF5/current/src/hdf5-1.8.13.tar.gz
    tar xzf hdf5-1.8.13.tar.gz
    wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4.3.2.tar.gz
    tar xzf netcdf-4.3.2.tar.gz
    wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-4.2.tar.gz
    tar xzf netcdf-fortran-4.2.tar.gz

Change to the HDF source directory.  Add flags to enable the C++ and Fortran API to be built in your `configure` statement

    cd $WORK/hdf5-1.8.13
	./configure --prefix=$PREFIX --enable-fortran --enable-fortran2003 --enable-cxx --enable-parallel
	make -j8 && make check && make install

Then install the netcdf C library first

    cd $WORK/netcdf-4.3.2
	./configure --prefix=$PREFIX && make -j8 && make check && make install

and later the fortran library. Issue
    
    cd $WORK/netcdf-fortran-4.2
    ./configure --prefix=$PREFIX && make -j8 && make check && make install

You now have your system ready to be used with ESMF and recent Fortran 2003 requirements.

