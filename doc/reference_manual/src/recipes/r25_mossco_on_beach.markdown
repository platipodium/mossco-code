# Recipe #25: MOSSCO on Beach

Beach is the testing HPC environment provided by Colorado University and CSDMS for integrated model development.  

You can apply for an account by first joining CSDMS and then applying for their HPC access.

## Preparing your environment

(also see instructions at http://csdms.colorado.edu/wiki/HPCC_guidelines)

Choose your shell

		source

Load the appropriate modules

		module purge
		module load netcdf
		module load doxygen
		module load python
		module load cmake

		export NETCDF=NETCDF4
		export NETCDF_VERSION=$NETCDF
		export FORTRAN_COMPILER=GFORTRAN

This example also assumes that you have downloaded or `git clone`ed MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`, and the same for the setup directory pointed to by `$MOSSCO_SETUPDIR`.

Download and install ESMF with the script provided by MOSSCO

		export ESMF_DIR=$HOME/esmf
		cd $ESMF_DIR
		bash $MOSSCO_DIR/scripts/installation/install_esmf.sh

## Obtaining external sources and compiling MOSSCO

		make -C ${MOSSCO_DIR} external all

## Deployment of an example

(this is only one of the many examples)

		cd $MOSSCO_SETUPDIR/sns
		make ref3d

		ln -sf Forcing/meteofiles_clm_jureca.dat meteofiles_gen.dat  # link meterological files
		ln -sf Parallel/par_setup.192p.7x8.dat par_setup.dat	# setup getm for 192 processors


		#mossco -rbn192 getm
		#qsub slurm.sh @todo add Torque system instructions
