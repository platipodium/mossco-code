# Recipe #26: Installation on a non-admin SuSE Linux system

Many users do not have admin rights on their computer systems, here we show for a `SUSE Linux12.2` the necessary steps.

## Software packages

Our test system came preinstalled with the following software packages:

- preinstalled gcc-4.8.5
- preinstalled svn
- preinstalled hdf5
- preinstalled netcdf-4.3.1
- preinstalled netcdf-fortran

So the IT department is needed to install the following, in addition:

- cmake
- git
- doxygen
- OpenMPI oder MPICH2

And to reinstall netcdf-fortran since `netcdf.mod` was missing.

## First installation without MPI and manual netcdf-fortran

netcdf-fortran was installed manually and the `LD_LIBRARY_PATH` was adjusted. Also, the local `nf-config` location was prepended to the `PATH` environment variable.

        export ESMF_DIR=$HOME/opt/esmf
        mkdir -p $ESMF_DIR
        git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf $ESMF_DIR
        cd $ESMF_DIR

        cp $MOSSCO_DIR/scripts/installation/install_esmf_versions.sh .

Edit lines 7, 8, 13 to set `COMPS=gfortran`, `COMMS=mpiuni`, and
`ESMF_INSTALL_PREFIX=$HOME/opt`

        bash install_esmf_versions.sh

This installation might fail (with `netcdf`).  Change the following lines in the file
$HOME/.esmf_Linux.gfortran.64.mpich2.ESMF_7_1_0_beta_snapshot_22'

        export ESMF_NETCDF_INCLUDE="$(nf-config --includedir) -I$(nc-config --includedir)"
        export ESMF_NETCDF_LIBPATH="$HOME/opt/lib -L/usr/local/lib"

Then try to reinstall ESMF

        cd $ESMF_DIR
        source $HOME/.esmf_Linux.gfortran.64.mpich2.ESMF_7_1_0_beta_snapshot_22
        make && make install
