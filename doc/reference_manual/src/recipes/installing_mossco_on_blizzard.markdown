# Recipe 7: Installing MOSSCO on blizzard

Blizzard is the high performance computing system at the DKRZ.

## User environment

Assuming you belong to the group gg0877, login and define the following environment variables on <ssh://blizzard.dkrz.de>

	export KSE_WRKDIR=/work/gg0877
	export OS=$(uname)
	export PATH=$PATH:$KSE_WRKDIR/bin_$OS
	export MODULEPATH=${MODULEPATH}:$KSE_WRKDIR/modules_KSE_$OS
	export PYTHONPATH=$PYTHONPATH:$KSE_WRKDIR/local_$OS/lib/python2.7/site-packages


## Using preinstalled ESMF
Now load the necessary modules and prepare ESMF

	source /work/gg0877/software/ESMF/esmf_6.3.0_AIXrc

This script will perform the following actions:

	export ESMF_DIR=/work/gg0877/software/ESMF/esmf_6.3.0/esmf
	export ESMF_INSTALL_PREFIX=/work/gg0877/software/ESMF/install_dirs/AIX/esmf_6_3_0
	export ESMFMKFILE=${ESMF_INSTALL_PREFIX}/lib/libO/AIX.default.64.mpi.default/esmf.mk
	export ESMF_MPIRUN=mpirun.aix.p6
	export ESMF_NETCDF=split
	module load KSE_NETCDF/4.1.3-aix
	export ESMF_NETCDF_INCLUDE=/sw/aix61/netcdf-4.1.3/include
	export ESMF_NETCDF_LIBPATH=/sw/aix61/netcdf-4.1.3/lib

	export ESMF_F90LINKPATHS="-L/sw/aix61/hdf5-1.8.8/lib -L/sw/aix53/zlib-1.2.6/lib -L/sw/aix53/szip-2.1/lib"
	export ESMF_CXXLINKPATHS="-L/sw/aix61/hdf5-1.8.8/lib -L/sw/aix53/zlib-1.2.6/lib -L/sw/aix53/szip-2.1/lib"
	export ESMF_F90LINKLIBS="-lhdf5_hl -lhdf5 -lz -lsz"
	export ESMF_CXXLINKLIBS="-lhdf5_hl -lhdf5 -lz -lsz"
	
## Installing MOSSCO

You should now be able to install MOSSCO just as on any other system.

> Not really, it does not work yet...


## Patching ESMF

	--- a/build_config/AIX.default.default/build_rules.mk
	+++ b/build_config/AIX.default.default/build_rules.mk
	@@ -32,7 +32,7 @@ ESMF_F90DEFAULT         = mpxlf90_r
	 ESMF_F90LINKLIBS       += -lmpi_r
 	ESMF_CXXDEFAULT         = mpCC_r
 	ESMF_CXXLINKLIBS       += -lmpi_r
	-ESMF_MPIRUNDEFAULT      = mpirun.pc604
	+ESMF_MPIRUNDEFAULT      = mpirun.blizzard
 	else
	
	--- a/src/Infrastructure/Util/src/ESMF_FortranWordsize.cppF90
	+++ b/src/Infrastructure/Util/src/ESMF_FortranWordsize.cppF90
	@@ -260,6 +260,7 @@ end subroutine
	       implicit none @\
	  @\
	       type wrapper @\
	+        sequence @\
	         mname (ESMF_KIND_##mtypekind), dimension(mdim), 	pointer :: ptr @\
	       end type @\
	  @\


	
## Todo

- rename install dirs and adapt the rc files
   done for all

- recompile aix esmf_7.0.0 and check for c++ problems
   done, mail to esmf-support sent
   additional tests done to test hint from Walter Spector (ESMF)

- compile and check esmf_7.0.0 on wizard
   done

- checkout 6.3.0 from sourceforge repository and replace the beta version
   done

- check group write access for all data
   done

- run mossco test programs with the new compiled library
	