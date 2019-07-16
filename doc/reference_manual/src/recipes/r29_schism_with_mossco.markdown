# Recipe #29: MOSSCO with SCHISM

SCHISM is the open source Semi-implicit Cross-scale Hydroscience Integrated System Model hosted by the Virgina Institute for Marine Science.  Its web site is
http://ccrm.vims.edu/schismweb, its documentation
http://ccrm.vims.edu/schismweb/SCHISM_v5.6-Manual.pdf . Next to SCHISM itself, you will need its Earth System Modeling Framework (ESMF) Component Model Interface (CMI) and a Framework for Aquatic Biogeochemistry (FABM) from the official FABM repository

## Prepare your environment

As with MOSSCO itself, first consider where to put SCHISM and point to it with environment variables.  Three variables are needed

1. `SCHISM_DIR` : points to location of the *build* directory of SCHISM
2. `SCHISM_ESMF_DIR` : points to the locatino of the *source* directory of the ESMF interface for SCHISM.
3. `PARMETIS_DIR` : points to the location of the ParMETIS library

In the following instructions, we assume that `$SCHISM_DIR` is a directory parallel to the source directory of SCHISM, its ESMF interface, and FABM, for example

		export SCHISM_DIR=/my/path/to/schism/build
		export SCHISM_ESMF_DIR=$SCHISM_DIR/../esmf
		export PARMETIS_DIR=$SCHISM_DIR/../trunk/src/ParMetis-3.1-Sep2010

> On HPC systems, you can often load ParMETIS as a library with `module load ParMETIS`, you then have to set the path to this library in the environment variable `$PARMETIS_DIR`, e.g. on JUWELS

		module load ParMETIS
		export PARMETIS_DIR=/gpfs/software/juwels/stages/2019a/software/ParMETIS/4.0.3-ipsmpi-2019a/lib

## Download SCHISM

Instructions for downloading and installing SCHISM are provided in their documentation.  The current version can be downloaded for free and without registration with subversion.  SCHISM's Component Model Interface (CMI) for the Earth System Modeling Framework (ESMF) is developed in a separate branch which you can also obtain through subversion.

		cd $SCHISM_DIR/..
		svn co http://anonymous@columbia.vims.edu/schism/trunk
		svn co http://anonymous@columbia.vims.edu/schism/esmf
		git clone --depth=1 https://github.com/fabm-model/fabm.git

> If you would like to have write access to SCHISM, please contact the lead developer Joseph Zhang and log in with your user name.

## Compile SCHISM

SCHISM has `CMake` and `netCDF` as dependencies and an MPI Fortran compiler (you may need to adjust your MPI compilers in the `cmake` command below), so make sure you have those installed and available in your environment.  SCHISM can be built with FABM support from the offical FABM repository (currently not the one distributed with MOSSCO).  The ESMF interface has the ESMF library as a dependency, which needs to be pointed to by the environment variable `$ESMFMKFILE`.

> If you do not have the ParMETIS library in your system, compile this library within the SCHISM source tree *before* compiling SCHISM

		cd $PARMETIS_DIR
		make

Then compile SCHISM and use `CMake` define flags for providing the location to `netCDF`

		cd $SCHISM_DIR
		cmake ../trunk/src -DCMAKE_BUILD_TYPE=RelWithDebInfo \
		  -DNetCDF_FORTRAN_DIR=`nf-config --prefix` \
	    -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_CXX_COMPILER=mpicc \
	    -DNetCDF_C_DIR=`nc-config --prefix` -DTVD_LIM=SB -DUSE_FABM=OFF
		make pschism combine_hotstart7 combine_output11
		mv bin/pschism bin/pschism_no_fabm

		cmake ../trunk/src -DCMAKE_BUILD_TYPE=RelWithDebInfo \
		  -DNetCDF_FORTRAN_DIR=`nf-config --prefix` \
	    -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_CXX_COMPILER=mpicc \
	    -DNetCDF_C_DIR=`nc-config --prefix` -DTVD_LIM=SB -DUSE_FABM=ON \
	    -DFABM_BASE=../fabm
		make pschism combine_hotstart7 combine_output11
		mv bin/pschism bin/pschism_fabm

> On some systems (like MISTRAL), the intel mpi executables are named `mpiifort` and `mpiicc` instead.

Your SCHISM executables are now `$SCHISM_DIR/bin/pschism_no_fabm` and `$SCHISM_DIR/bin/pschism_fabm`

## Compile a MOSSCO/SCHISM example

		cd $MOSSCO_DIR/examples/esmf/schism
		make install
		make

If there is a conflict with `LAPACK` symbols, then the following might help to fix This

		objcopy --redefine-sym xerbla_=schism_lap_xerbla_ $SCHISM_DIR/lib/libhydro.a
