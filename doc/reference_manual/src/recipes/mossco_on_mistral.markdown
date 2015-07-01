# Recipe #14: MOSSCO on mistral

Mistral is the supercomputer at the German Climate Computing Centre (DKRZ) from July 2015.  It succeeds the blizzard system.

Mistral is a RedHat6 Linux system with 1500 nodes with each 2x12 core processors.

## Preparing your toolchain

This recipe gives instructions for the intel14/openmpi toolchain.  On mistral, there are many more compilers (gcc4, gcc5, nag) and MPI installations (bull, intel, mvapich, openmpi) available.

        module purge
        module load intel/14.0.3 openmpi/1.8.4-intel14
        module load python

## Downloading MOSSCO and obtaining external sources

This recipe also assumes that you have downloaded or `git clone`d MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`, and have `git clone`d the MOSSCO setups into `$MOSSCO_SETUPDIR`.

        export MOSSCO_DIR=/my/path/to/mossco/code
        export MOSSCO_SETUPDIR=/my/path/to/mossco/code

.. and the same for the setups

        mkdir -p $MOSSCO_SETUPDIR
        git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR
        make -C $MOSSCO_SETUPDIR external # to obtain editscenario

## Preparing shortcuts

        mkdir -p $HOME/opt/bin
        ln -sf /sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/bin/nf-config $HOME/opt/bin
        ln -sf /sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/bin/nf-config $HOME/opt/bin
        ln -sf $MOSSCO_DIR/src/scripts/mossco.sh $HOME/opt/bin/mossco
        ln -sf $MOSSCO_DIR/src/scripts/postprocess/stitch_tiles.py $HOME/opt/bin/stitch
        export PATH=$HOME/opt/bin:$PATH

Note that aliases don't reliably work  for `nf-config`, so we use soft links here.

## Installing py-yaml

## Installing ESMF

        export ESMF_DIR=/my/path/to/netcdf
        git clone http://git.code.sf.net/p/esmf/esmf $ESMF_DIR

        export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE
        export ESMF_OS=Linux
        export ESMF_OPTLEVEL=2
        export ESMF_LAPACK=internal
        export ESMF_COMM=openmpi
        export ESMF_INSTALL_PREFIX=$HOME/opt
        export ESMF_BOPT=g
        export ESMF_SITE=ESMF_7_0_0_beta_snapshot_49
        export ESMF_COMPILER=intel
        export ESMF_NETCDF=split
        export ESMF_NETCDF_LIBS=$(nf-config --flibs)
        export ESMF_NETCDF_INCLUDE="$(nc-config --includedir) -I$(nf-config --includedir)"

        cd $ESMF_DIR; make lib install

## Installing py-yaml

Download and install py-yaml

        mkdir -p $HOME/opt/PYTHON/
        cd $HOME/opt/PYTHON/
        wget http://pyyaml.org/download/pyyaml/PyYAML-3.11.tar.gz
        tar xzf PyYAML-3.11.tar.gz
        cd PyYAML-3.11
        python setup.py install --prefix=$HOME/opt

## Last steps

        export PYTHONPATH=$PYTHONPATH:$HOME/opt/lib/python2.7/site-packages
        export ESMFMKFILE=$HOME/opt/lib/libg/Linux.intel.64.openmpi.ESMF_7_0_0_beta_snapshot_49/esmf.mk

        export NETCDF=NETCDF4
        export NETCDF_VERSION=$NETCDF
        export FORTRAN_COMPILER=IFORT

Now you're ready to build and run MOSSCO applications by continuing with the [using_mossco_sh] recipe
