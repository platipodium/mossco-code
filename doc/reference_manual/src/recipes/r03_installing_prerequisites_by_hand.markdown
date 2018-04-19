# Recipe #03: install your own software prerequisites (gcc, openmpi, netcdf, cmake)

This document describes how to install all the prerequisites
for running ESMF and MOSSCO not with a package manager, but
by a *manual install* of all required software packages.

These instructions were successfully tested on

- Linux 2.6.39-400.214.3.el5uek SMP x86_64 24 Intel(R) Xeon(R) CPU E5645 @ 2.40GHz
- Linux 3.13.0-24-generic SMP x86_64 12 Intel(R) Xeon(R) CPU E5649  @ 2.53GHz
- Linux 2.6.16.60-0.37_f594963d_lustre.1.8.0.1-smp  SMP x86_64  4 Dual Core AMD Opteron(tm) Processor 285

## Assumptions

The instructions below assume, that you have already present in your system the facilities to `configure` and `make` software, i.e., that a recent enough version of `automake` and `autoconf` and a system C compiler are already present.

## Installation directories

These instructions install all the software in the directory `PREFIX=$HOME/opt`, i.e., the libraries in `$PREFIX/lib`, header files in `$PREFIX/include`, and binaries in `$PREFIX/bin`.  Your choice may
be different.

        export PREFIX=$HOME/opt

If you have root access on your system, we recommend to set `PREFIX=/opt/gcc49` or similar, as the installed software will be compiler-specific.  To avoid having to use `sudo` privileges in your installation, you should make sure that the directory `$PREFIX` is owned by your local user.  If not, ensure this with

        sudo chown -R your-user-name $PREFIX

To use the software, please add the binary directory to your search path and the library directory to your library search path

        export PATH=$PREFIX/bin:$PATH
        export LD_LIBRARY_PATH=$PREFIX/lib:$LD_LIBRARY_PATH

## Software versions
In this tutorial, we chose a working set of software versions; by no means we intend to restrict you to using these specific versions.  You would have to adjust download URLs and paths accordingly, if you chose alternate versions.

In this example, we chose to download the software to the directory `WORK=$HOME/Downloads`, unpack it there, build it there, and later install it to `$PREFIX`.

        export WORK=$HOME/Downloads


## Installing the new GNU Compiler Collection

Because of insufficient support for the FORTRAN 2003 standard, and because of bugs in the 4.7.x series
of GCC, MOSSCO requires GCC from version 4.8.0 onwards.  You can install this compiler yourself from source, and we recommend to use the most
recent release version (gcc-6.3.0 at the time of writing this recipe).

        cd $WORK
        wget ftp://ftp.gwdg.de/pub/misc/gcc/releases/gcc-6.3.0/gcc-6.3.0.tar.bz2
        tar xpjf gcc-6.3.0.tar.bz2
        cd gcc-6.3.0

GCC itself requires three software packages GMP, MPFR, and MPC to be installed (in this order).  You can do this manually, but there's
a good chance to fail since the gcc `./configure` script is not smart enough yet.  Fortunately, gcc comes with a a script to take care
of its dependencies [see this discussion](http://gcc.gnu.org/wiki/FAQ#configure).

In the GCC source directory, run

        ./contrib/download_prerequisites

Create a directory next to the gcc source directory

        mkdir ../gcc-build ; cd ../gcc-build

From within this directory, run the `../gcc-6.3.0/configure` script and make

        ../gcc-6.3.0/configure --prefix=$PREFIX
        make -j8 && make check && make install

Compiling the compiler takes a long time (go do something else for
half an hour to several hours).  But once you are done, you can use
your new shiny gcc compiler (and also gfortran and other languages) from `$PREFIX/bin`

## Installing OpenMPI

For support of parallel computing, the installation of a message passing interface (MPI) is required, which has to be in conjunction with the compiler built in the previous step. We recommend to use the most recent release version of openMPI (v2.1.0 at the time of writing this recipe).

        cd $WORK
        wget https://www.open-mpi.org/software/ompi/v2.1/downloads/openmpi-2.1.0.tar.bz2
        tar xpjf openmpi-2.1.0.tar.bz2
        cd openmpi-2.1.0

Issue the usual

        ./configure --prefix=$PREFIX
        make -j8 && make check && make install

> You might need the additional option `--disable-multilib` to make this work if your system does not have 32 bit libraries.

From now on, *do not* use gcc/gfortran/g++ as your compilers, but *use mpifort/mpicc/mpiCC* as your compilers.  You can tell this to your system
by setting

        export FC=mpifort
        export CC=mpicc
        export CXX=mpiCC

(or use equivalent `csh` syntax, e.g. `setenv FC mpifort`).  Note that with recent versions of OpenMPI, the commands `mpif90` and `mpif77` are deprecated in favor of `mpifort`

## Installing netCDF

The current netCDF comes in three packages, one for C, one for Fortran (and one for C++, which is *not* needed).  As a requirement for new features of the netCDF-4 standard, the HDF-5 library is required and must be installed first.

To get all the software, issue the following:

        cd $WORK
        wget https://support.hdfgroup.org/ftp/HDF5/current/src/hdf5-1.10.0-patch1.tar.bz2
        tar xpjf hdf5-1.10.0-patch1.tar.bz2
        wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4.4.1.1.tar.gz
        tar xzf netcdf-4.4.1.1.tar.gz
        wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-4.4.4.tar.gz
        tar xzf netcdf-fortran-4.4.4.tar.gz

Change to the HDF source directory.  Add flags to enable the Fortran API to be build in your `configure` statement

        cd $WORK/hdf5-1.10.0-patch1
        ./configure --prefix=$PREFIX --enable-fortran --enable-fortran2003 --enable-parallel
        make -j8 && make check && make install

> A user reported the additional need for `--enable-unsupported` when also using `--enable-cxx`.  Errors have also been reported with `make check`.

Then install the netcdf C library first

        cd $WORK/netcdf-4.4.1.1
        ./configure --prefix=$PREFIX
        make -j8 && make check && make install

> A user reported that she needed to add the `libdl` library at link time

and later the fortran library. Issue

        cd $WORK/netcdf-fortran-4.4.4
        ./configure --prefix=$PREFIX
        make -j8 && make check && make install

You now have your system ready to be used with ESMF and recent Fortran 2003 requirements.

## Installing ESMF

Decide on a directory where to put the ESMF source, and set the environment variable `ESMF_DIR` to point to this directory. In this example, we chose to download the software to the directory `$WORK/esmf-code` and as the installation should take place in the same directory as was previously defined in `$PREFIX` we set the environment variable `ESMF_INSTALL_PREFIX` accordingly.

        export ESMF_DIR=$WORK/esmf-code
        export ESMF_INSTALL_PREFIX=$PREFIX
        git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf $ESMF_DIR
        cd $ESMF_DIR

During the installation process, ESMF requires a couple of environment variables to be set. (See the ESMF manual for a complete list of ESMF environment variables and their relevance.) At the time of writing this recipe, we further recommend to check out the tagged version `ESMF_7_1_0_beta_snapshot_22` which has been prooved to fullfil MOSSCO requirements. Both steps have been automated using and adjusting the script `install_esmf_versions.sh`, which comes along with the Mossco-code.
To install ESMF, just copy this file to `$ESMF_DIR`

        cp $MOSSCO_DIR/scripts/installation/install_esmf_versions.sh .

,edit lines 7, 8 to set `COMPS=gfortran`, `COMMS=openmpi` and invoke

        bash install_esmf_versions.sh


## Installing CMake (prerequisite for using FABM component)

Recent versions of FABM require `CMake`,  an alternative to the autotools sytem (`configure`).  GETM, GOTM, and possibly MOSSCO will also move in the future to this configuration program.

If you do not have CMake, it is quite easy to install

        cd $(WORK)
        git clone git://cmake.org/cmake.git
        cd cmake
        git checkout --track -b release origin/release
        ./configure --prefix=$PREFIX
        make && make install
