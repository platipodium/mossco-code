# Recipe #6 Installing the ROMS ocean model

The Regional Ocean Modeling Sytem (ROMS) is a free-surface, terrain-following, primitive equations ocean model.  

## Obtaining ROMS

1. Register with ROMS on their web site  https://www.myroms.org/index.php?page=login and wait a couple of days for your account information
2. Download ROMS via `svn` or via `git`

## Installing ROMS

As prerequisites, you need `svn`, `GNU Make`, `Perl` and a Fortran 90 compiler.  For the following, we assume that you have the source code of ROMS in the directory `$ROMS_DIR`

Then, define environment variables that are used in `$ROMS_DIR/Compilers/Makefile.XXX`.  On my system, I set

	export USE_NETCDF4=true
	export USE_MPI=true
	export USE_MPIF90=on
	export which_MPI
	export USE_ESMF=true
	export FORT=gfortran
	
The latter relies on on a valid ESMF source pointed to by `ESMF_DIR`. [todo: this should be patched to point to the installed version of ESMF]

The current version of ROMS does not operate well with the current version of ESMF, you need to apply a patch


Then build ROMS (it will build with the sample `UPWELLING` test case)

	cd $ROMS_DIR
	make -j4