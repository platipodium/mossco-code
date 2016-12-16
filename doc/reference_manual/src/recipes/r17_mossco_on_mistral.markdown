# Recipe #17: MOSSCO on mistral

Mistral is the supercomputer at the German Climate Computing Centre (DKRZ) from July 2015.  It succeeds the Blizzard system.

Mistral is a RedHat6 Linux system with 1500 nodes with each 2x12 core processors (in the "compute" partition), or 2x18 core processors (in the "compute2" partition).

## Preparing your toolchain

This recipe gives instructions for the gcc49/openmpi toolchain.  On mistral, there are many more or more recent compilers (gcc4, gcc5, nag, intel16) and MPI installations (bull, intel, mvapich, openmpi) available, but not with a preinstalled ESMF.  We are waiting for a preinstallation of ESMF with the
recommended compiler systems intel/bullxmpi and intel/intelmpi.

        module purge
        module load nco ncview python

        module load gcc/4.9.2
        module load openmpi/1.8.4-gcc49
        module load esmf

        export ESMFMKFILE=/sw/rhel6-x64/esmf-7.0.0-parallel-openmpi-gcc49/lib/esmf.mk
        export FORTRAN_COMPILER=GFORTRAN

There is no module for netcdf Fortran, so these paths need to be set by the userRoutine

        mkdir -p $HOME/opt/bin
        export PATH=$HOME/opt/bin:$PATH
        alias nf-config=/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-gcc49/bin/nf-config
        alias nc-config=/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-gcc49/bin/nc-config
        ln -sf /sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-gcc49/bin/nf-config $HOME/opt/bin
        ln -sf /sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-gcc49/bin/nc-config $HOME/opt/bin

        export NETCDF=NETCDF4
        export NETCDF_VERSION=$NETCDF

Note that aliases don't reliably work  for `nf-config`, so we use soft links here.

## Downloading MOSSCO and obtaining external sources

This recipe also assumes that you have downloaded or `git clone`d MOSSCO into a directory referred to by the environment variable `$MOSSCO_DIR`, and have `git clone`d the MOSSCO setups into `$MOSSCO_SETUPDIR`.

        export MOSSCO_DIR=/my/path/to/mossco/code
        export MOSSCO_SETUPDIR=/my/path/to/mossco/code

.. and the same for the setups

        mkdir -p $MOSSCO_SETUPDIR
        git clone git://git.code.sf.net/p/mossco/setups $MOSSCO_SETUPDIR
        make -C $MOSSCO_SETUPDIR external # to obtain editscenario

Lastly, install the shortcut to MOSSCO's run and stitch scripts

        ln -sf $MOSSCO_DIR/scripts/mossco.sh $HOME/opt/bin/mossco
        ln -sf $MOSSCO_DIR/scripts/postprocess/stitch_tiles.py $HOME/opt/bin/stitch

## Installing or using py-yaml

### Using a preinstalled py-yaml

If you are part of the ecosystem modeling HPC group at DKRZ, then you will have read rights
to the installation located at

        export PYTHONPATH=$PYTHONPATH:/pf/g/g260077/opt/lib/python2.7/site-packages

### Download and install py-yaml

        mkdir -p $HOME/opt/PYTHON/
        cd $HOME/opt/PYTHON/
        wget http://pyyaml.org/download/pyyaml/PyYAML-3.11.tar.gz
        tar xzf PyYAML-3.11.tar.gz
        cd PyYAML-3.11
        python setup.py install --prefix=$HOME/opt

        export PYTHONPATH=$PYTHONPATH:$HOME/opt/lib/python2.7/site-packages

Now you're ready to build and run MOSSCO applications by continuing with the [[using_mossco_sh]] recipe
