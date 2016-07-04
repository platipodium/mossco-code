# Recipe #17: MOSSCO on mistral

Mistral is the supercomputer at the German Climate Computing Centre (DKRZ) from July 2015.  It succeeds the blizzard system.

Mistral is a RedHat6 Linux system with 1500 nodes with each 2x12 core processors (in the "compute" partition), or 2x18 core processors (in the "compute2" partition).

## Preparing your toolchain

This recipe gives instructions for the intel14/openmpi toolchain.  On mistral, there are many more compilers (gcc4, gcc5, nag) and MPI installations (bull, intel, mvapich, openmpi) available.

        module purge
        module load openmpi/1.8.4-intel14
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
        ln -sf $MOSSCO_DIR/scripts/mossco.sh $HOME/opt/bin/mossco
        ln -sf $MOSSCO_DIR/scripts/postprocess/stitch_tiles.py $HOME/opt/bin/stitch
        export PATH=$HOME/opt/bin:$PATH

Note that aliases don't reliably work  for `nf-config`, so we use soft links here.

## Installing ESMF

Use the script `$MOSSCO_DIR/scripts/installation/install_esmf_versions.sh`.  Edit
the entries for `COMPILER` and `COMM` to `intel` and `openmpi`, respectively.

        export ESMF_DIR=/my/path/to/esmf
        cd $ESMF_DIR;
        $MOSSCO_DIR/scripts/installation/install_esmf_versions.sh


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
