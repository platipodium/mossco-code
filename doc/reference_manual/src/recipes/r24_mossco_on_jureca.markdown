# Recipe #24: MOSSCO on Jureca

Jureca (Juelich Research on Exascale Cluster Architectures) is one of the current supercomputers at FZ Jülich.
It is based on Intel Xeon E5-2680 v3 Haswell CPUs with 12 cores per CPU and dual sockets. 

You can apply for a testaccount by contacting their support at <dispatch.jsc@fz-juelich.de>.

## Preparing your environment

Load the appropriate modules of the prior "stage" (the current one does not have all necessary software yet)

		module purge
		module use /usr/local/software/jureca/OtherStages
		module load Stages/2016a
		module load Intel ParaStationMPI
		module load ESMF/7.0.0
		module load Python/2.7.11
		module load netcdf4-python/1.2.4-Python-2.7.11
		module load basemap/1.0.7-Python-2.7.11
		module load PyYAML/3.11-Python-2.7.11
		module load git Subversion
		module load netCDF-Fortran
		module load Doxygen
		module load NCO  ncview
		module load CMake

		export ESMFMKFILE=/usr/local/software/jureca/Stages/2016a/software/ESMF/7.0.0-intel-para-2016a/lib/esmf.mk

		export NETCDF=NETCDF4
		export NETCDF_VERSION=$NETCDF
		export FORTRAN_COMPILER=IFORT

This example also assumes that you have downloaded or `git clone`ed MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`, and the same for the setup directory pointed to by `$MOSSCO_SETUPDIR`.

## Obtaining external sources and compiling MOSSCO

		make -C ${MOSSCO_DIR} external all

## Deployment of an example

(this is only one of the many examples)

		cd $MOSSCO_SETUPDIR/sns
		make ref3d

		ln -sf Forcing/meteofiles_clm_jureca.dat meteofiles_gen.dat  # link meterological files
		ln -sf Parallel/par_setup.192p.7x8.dat par_setup.dat	# setup getm for 192 processors

		mossco -rbn192 getm
		sbatch slurm.sh

## Getting into the queue faster

We currently automatically only use the `compute2` partition of Jureca.  You can get into the queue faster by adjusting the generated `slurm.sh`

		#!/bin/bash
		#SBATCH --job-name=my_job
		#SBATCH --partition=compute,compute2

Make sure that  `--ntasks` is set to a multiple of the cpus on **both** partitions, i.e. 24 and 36.  So 72, 144, 288 would work in this setup.

With the `mossco` script, you can also use the `-q` option to select a queue/partition.

		mossco -n288 -q "compute2,compute" getm
