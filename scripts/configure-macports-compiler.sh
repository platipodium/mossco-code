#!/usr/bin/env bash

MPI=$1
COMPILER=$2

if [ -z ${COMPILER} ]; then COMPILER=gfortran
fi

if [ -z ${MPI} ]; then MPI=mpich
fi


case ${MPI} in
openmpi )
  echo "Using OpenMPI"
  ;;
mpich )
  echo "Using MPICH"
  ;;
* )
  echo "Invalid MPI choice '${MPI}', valid choices [openmpi, mpich]."
  exit 1
  ;;
esac

CCOMPILER=${COMPILER}

case ${COMPILER} in
gfortran)
  echo "Using gfortran/gcc9"
  CCOMPILER=gcc9
  ;;
# gfortranclang)
#   echo "Using gfortran/clang"
#   CCOMPILER=clang90
#   ;;
# flang)
#   echo "Using flang/clang"
# ;;
*)
  echo "Invalid compiler choice '${COMPILER}', valid choices [gfortran]"
  exit 1
;;
esac

# Update your ports system
port selfupdate

# Install the base compiler and set this version as the default version
case ${CCOMPILER} in
gcc*)
  port install ${CCOMPILER} gcc_select
  port select --set gcc mp-${CCOMPILER}
  ;;
clang90)
  port install clang-9.0 clang_select
  port select --set clang mp-clang-9.0
esac

# Install MPI for your base compiler, and set this as the default
port clean ${MPI}-${CCOMPILER}
port install ${MPI}-${CCOMPILER}
port clean ${MPI}-default
port install ${MPI}-default +${CCOMPILER}

port select --set mpi ${MPI}-${CCOMPILER}-fortran

# Clean all mpi/compiler dependent ports necessary for esmf
port clean hdf5 netcdf netcdf-fortran esmf

port install hdf5 +cxx+hl+${MPI}
port install netcdf +cdf5+dap+${MPI}+netcdf4

# netcdf-fortran accepts gcc9 (and prior versions) only
port install netcdf-fortran +${CCOMPILER}+${MPI}

# esmf accepts gcc9 (and prior versions) only
port install esmf +accelerate+${COMPILER}+${MPI}

# You might want to use mpi from python with the same settings
port clean py37-mpi4py py37-netcdf4
port install py37-netcdf4 +${COMPILER}+${MPI}
port install py37-mpi4py +${COMPILER}+${MPI}
