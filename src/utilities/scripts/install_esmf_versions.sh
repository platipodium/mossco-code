#!/bin/bash


export TAGS="ESMF_6_3_0r ESMF_7_0_0_beta_snapshot_06 ESMF_6_3_0rp1_beta_snapshot_07 ESMF_5_2_0rp3 ESMF_5_3_1_beta_snapshot_18"
export COMPS="gfortran" #pgi intel"
export COMMS="mpich2 mpiuni"

export ESMF_DIR=${HOME}/devel/ESMF/esmf-code
export ESMF_INSTALL_PREFIX=/opt/esmf

cd $ESMF_DIR && git pull

#echo y        | module clear

for C in $COMMS ; do
  echo "Iterating for Communicator $C ============================================="
  export ESMF_COMM=$C

  for G in $COMPS; do
    echo "Iterating for Compiler $G ============================================="
    export ESMF_COMPILER=$G

    if [ $G == intel ]; then
      source /opt/intel/bin/ifortvars.sh intel64
      source /opt/intel/bin/iccvars.sh intel64

      export MPI_PATH=/opt/intel/mpich3
      export PATH=$MPI_PATH/bin:$PATH
      export NETCDF_PATH=/opt/intel/netcdf4
      export ESMF_NETCDF_INCLUDE=${NETCDF_PATH}/include
      export ESMF_NETCDF_LIBS=" -L ${NETDF_PATH}/lib -lnetcdf"
    else    
      export ESMF_NETCDF_INCLUDE="`nc-config --includedir`"
      export ESMF_NETCDF_LIBS="`nc-config --flibs` -lnetcdf_c++ -lnetcdf"
    fi
#    echo y  | module clear
#    module load ${ESMF_COMPILER} || continue
#    module load openmpi_ib || continue
#    module load netcdf/3.6.2 || continue
#    module list

    for T in $TAGS; do
       echo "Iterating for Tag $T ============================================="
       export ESMF_SITE=$T
       ln -sf $ESMF_DIR/build_config/Darwin.${ESMF_COMPILER}.default $ESMF_DIR/build_config/Darwin.${ESMF_COMPILER}.${ESMF_SITE} || continue
       git stash
       git stash drop
       git checkout  -f $T
       echo ln -sf $ESMF_DIR/build_config/Darwin.${ESMF_COMPILER}.default $ESMF_DIR/build_config/Darwin.${ESMF_COMPILER}.${ESMF_SITE} || continue

       echo ESMFMKFILE=$ESMF_INSTALL_PREFIX/lib/libg/Darwin.${ESMF_COMPILER}.64.${ESMF_COMM}.${ESMF_SITE}/esmf.mk
       #test -f $ESMFMKFILE && continue 

cat << EOT > $HOME/.esmf_${ESMF_COMPILER}.64.${ESMF_COMM}.${ESMF_SITE}
export ESMF_DIR=${ESMF_DIR}
export ESMF_BOPT=g
export ESMF_MOAB=OFF
export ESMF_OPTLEVEL=2
export ESMF_INSTALL_PREFIX=${ESMF_INSTALL_PREFIX}
export ESMF_LAPACK=internal
export ESMF_NETCDF=standard
export ESMF_NETCDF_INCLUDE=${ESMF_NETCDF_INCLUDE}
export ESMF_NETCDF_LIBS="${ESMF_NETCDF_LIBS}"
unset ESMF_XERCES
unset ESMF_PIO
export ESMF_SITE=$T
export ESMF_COMPILER=$G
export ESMF_COMM=$C
export ESMFMKFILE=$ESMF_INSTALL_PREFIX/lib/libg/Darwin.${ESMF_COMPILER}.64.${ESMF_COMM}.${ESMF_SITE}/esmf.mk
EOT
       export ESMF_STRING=${ESMF_COMPILER}.64.${ESMF_COMM}.${ESMF_SITE}
       .  $HOME/.esmf_${ESMF_STRING}
       #make distclean && make && make -j 8 check && make install
       make distclean 
       make -j4 lib && make install

       which install_name_tool || continue
       
       install_name_tool -id $ESMF_INSTALL_PREFIX/lib/libg/Darwin.${ESMF_STRING}/libesmf.dylib  $ESMF_DIR/lib/libg/Darwin.${ESMF_STRING}/libesmf.dylib  
        for F in $ESMF_INSTALL_PREFIX/bin/bing/Darwin.${ESMF_STRING}/* ; do
          install_name_tool -change $ESMF_DIR/lib/libg/Darwin.${ESMF_STRING}/libesmf.dylib $ESMF_INSTALL_PREFIX/lib/libg/Darwin.${ESMF_STRING}/libesmf.dylib  $F
        done
    done
  done
done

