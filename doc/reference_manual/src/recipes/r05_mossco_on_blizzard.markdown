# Recipe #05: MOSSCO on blizzard

Blizzard is the high performance computer at DKRZ.

## Loading modules

The IBM compiler version 14 is required for FABM 

    module load IBM/xlC12.1.0.7
	module load IBM/xlf14.1.0.8 
	module load NETCDF/4.2.1.1
	
## Alternative cmake

`cmake` is too old (2.8.4) on blizzard, you have to use a better version installed as

	/pf/g/g260077/opt/bin/cmake
	
## Installing FABM

Create a directory `$HOME/build/fabm` and execute `cmake` in this directory, don't forget to set a suitable Fortran 2003 compiler.

		/pf/g/g260077/opt/bin/cmake -DCMAKE_Fortran_COMPILER=xlf2003_r
		gmake
		

## Installing GOTM

Manipulate `$GOTMDIR/compilers/compiler.XLF` and replace xlf90 with mpxlf2003_r

	export FORTRAN_COMPILER=XLF
	export ARFLAGS='-X64 rv '
	export GOTMDIR=/pf/g/g260077/mossco/code/external/gotm/code
	export NETCDF_VERSION=NETCDF4
	
## Installing GETM

	export ARFLAGS='-X64 rv '
	export GOTMDIR=/pf/g/g260077/mossco/code/external/gotm/code
	export FORTRAN_COMPILER=XLF
	export GETMDIR=/pf/g/g260077/mossco/code/external/getm/code
	export NETCDF=NETCDF4 
	export GETM_PARALLEL=true
	export MPI=MPI
	
remove the line `#include fortran_version.h` from `$GETMDIR/include/cppdefs.h`


	
