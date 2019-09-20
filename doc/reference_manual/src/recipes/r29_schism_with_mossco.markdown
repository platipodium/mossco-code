# Recipe #29: MOSSCO with SCHISM

SCHISM is the open source Semi-implicit Cross-scale Hydroscience Integrated System Model hosted by the Virgina Institute for Marine Science.  Its web site is
http://ccrm.vims.edu/schismweb, its documentation
http://ccrm.vims.edu/schismweb/SCHISM_v5.6-Manual.pdf . Next to SCHISM itself, you will need its Earth System Modeling Framework (ESMF) Component Model Interface (CMI) and optionally the Framework for Aquatic Biogeochemistry (FABM).

## Prepare your environment

As with MOSSCO itself, first consider where to put SCHISM and point to it with environment variables.  Three variables are needed

1. `SCHISM_DIR` : points to the location of *source* directory of SCHISM
2. `SCHISM_BUILD_DIR` : points to location of the *build* directory of SCHISM
3. `SCHISM_ESMF_DIR` : points to the location of the *source* directory of the ESMF interface for SCHISM.

In the following instructions, we assume that `$SCHISM_BUILD_DIR` is a directory parallel to the source directory of SCHISM, its ESMF interface, and FABM, for example

		export SCHISM_DIR=/my/path/to/schism/schism-git
		export SCHISM_ESMF_DIR=$SCHISM_DIR/../schism-esmf
		export SCHISM_BUILD_DIR=$SCHISM_DIR/../build

Unlike earlier versions, the current SCHISM takes care of compiling the required ParMETIS library automatically.

## Download SCHISM

Instructions for downloading and installing SCHISM are provided in their documentation.  Development has recently been moved from subversion to git, and all SCHISM repositories are now hosted on https://github.com/schism-dev.

		mkdir -p ${SCHISM_DIR}/..
		cd ${SCHISM_DIR}/..
		git clone https://github.com/schism-dev/schism.git $SCHISM_DIR
		git clone https://github.com/schism-dev/schism-esmf.git $SCHISM_ESMF_DIR

## Compile SCHISM

SCHISM has `CMake` and `netCDF` as dependencies and an MPI Fortran compiler (you may need to adjust your MPI compilers in the `cmake` command below), so make sure you have those installed and available in your environment.  SCHISM can be built with FABM support from the offical FABM repository (currently not the one distributed with MOSSCO).  The ESMF interface has the ESMF library as a dependency, which needs to be pointed to by the environment variable `$ESMFMKFILE`. Compile SCHISM and use `CMake` define flags for providing the location to `netCDF`

		mkdir -p $SCHISM_BUILD_DIR && cd $SCHISM_BUILD_DIR
		cmake ${SCHISM_DIR}/src -DCMAKE_BUILD_TYPE=RelWithDebInfo \
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

Your SCHISM executables are now `$SCHISM_BUILD_DIR/bin/pschism_no_fabm` and `$SCHISM_BUILD_DIR/bin/pschism_fabm`

## Compile a MOSSCO/SCHISM example

		cd $MOSSCO_DIR/examples/esmf/schism
		make install
		make

If there is a conflict with `LAPACK` symbols, then the following might help to fix This

		objcopy --redefine-sym xerbla_=schism_lap_xerbla_ $SCHISM_DIR/lib/libhydro.a
