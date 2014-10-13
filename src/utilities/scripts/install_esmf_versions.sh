#!/bin/bash

TAGS=""

TAGS=ESMF_7_0_0_beta_snapshot_19
#TAGS=ESMF_6_3_0rp2_beta_snapshot_02
TAGS=ESMF_6_3_0rp1
#TAGS="ESMF_5_3_1_beta_snapshot_18
#TAGS=ESMF_4_0_1_beta_snapshot_01
#TAGS=ESMF_3_1_0rp5
export TAGS

COMPS="gfortran gfortranclang" # gfortran intel pgi gfortranclang pgigcc intelgcc
COMMS="mpich2" #"openmpi" #  mpiuni mpich2

test -n ${ESMF_DIR} || export ESMF_DIR = ${HOME}/devel/ESMF/esmf-code
cd $ESMF_DIR && git pull

test -n ${ESMF_INSTALL_PREFIX} || export ESMF_INSTALL_PREFIX=/opt/esmf
mkdir -p ${ESMF_INSTALL_PREFIX}/etc

export ESMF_OS=$(${ESMF_DIR}/scripts/esmf_os)
export ESMF_ABI=64

`which sed` && export SED=$(which sed)
`which gsed` && export SED=$(which gsed)
#export SED=$(which sed)

echo Using SED=${SED}
echo Using ESMF_OS=${ESMF_OS}
echo Using ESMF_INSTALL_PREFIX=${ESMF_INSTALL_PREFIX}

#echo y        | module clear

for C in $COMMS ; do
  echo "Iterating for Communicator $C ============================================="
  ESMF_COMM=$C

  for G in $COMPS; do
    echo "Iterating for Compiler $G ============================================="
    ESMF_COMPILER=$G

    ESMF_NETCDF_INCLUDE=$(nc-config --includedir)
    ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib
    
    if [ $G = intel ]; then
      source /opt/intel/bin/ifortvars.sh intel64
      source /opt/intel/bin/iccvars.sh intel64

      export MPI_PATH=/opt/intel/mpich3
      export PATH=$MPI_PATH/bin:$PATH
      NETCDF_PATH=/opt/intel/netcdf4
      ESMF_NETCDF_INCLUDE=${NETCDF_PATH}/include
      ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib
    else    
      ESMF_NETCDF_INCLUDE=$(nc-config --includedir)
      ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib
    fi
#    echo y  | module clear
#    module load ${ESMF_COMPILER} || continue
#    module load openmpi_ib || continue
#    module load netcdf/3.6.2 || continue
#    module list


    for T in $TAGS; do
       echo "Iterating for Tag $T ============================================="
       ESMF_SITE=$T
       ESMF_STRING=${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}
       git stash
       git stash drop
       git checkout  -f $T
       
       # Fix -lmpi_f77 on recent Darwin/MacPorts
       ${SED} -i 's#-lmpi_f77##g' ${ESMF_DIR}/build_config/Darwin.gfortran.default/build_rules.mk || continue
       
       ln -sf ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.default ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}
       
       echo ESMFMKFILE=$ESMF_INSTALL_PREFIX/lib/libg/$ESMF_STRING/esmf.mk
       #test -f $ESMFMKFILE && continue 

cat << EOT > $HOME/.esmf_${ESMF_STRING}
export ESMF_DIR=${ESMF_DIR}
export ESMF_BOPT=g
export ESMF_ABI=${ESMF_ABI}
export ESMF_MOAB=OFF
export ESMF_OPTLEVEL=2
export ESMF_INSTALL_PREFIX=${ESMF_INSTALL_PREFIX}
export ESMF_LAPACK=internal
export ESMF_NETCDF=split
export ESMF_NETCDF_INCLUDE=${ESMF_NETCDF_INCLUDE}
export ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_LIBPATH}
export ESMF_XERCES=standard
export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE
unset ESMF_PIO
export ESMF_SITE=$T
export ESMF_COMPILER=$G
export ESMF_COMM=$C
export ESMFMKFILE=$ESMF_INSTALL_PREFIX/lib/libg/${ESMF_STRING}/esmf.mk
EOT
       source $HOME/.esmf_${ESMF_STRING}
       cat $HOME/.esmf_${ESMF_STRING}
       echo $PATH

       #test -f $ESMFMKFILE || (make distclean && make -j12 lib && make install)
       (make distclean && make -j12 lib && make install)
       
       test -f $ESMFMKFILE || continue
       test -f ${ESMF_INSTALL_PREFIX}/lib/libg/${ESMF_STRING}/libesmf.a || continue 
       mkdir -p $ESMF_INSTALL_PREFIX/etc
       mv $HOME/.esmf_${ESMF_STRING} $ESMF_INSTALL_PREFIX/etc/${ESMF_STRING}
       
       # Fix dylib relocation on Darwin
       which install_name_tool || continue
       
       #install_name_tool -id $ESMF_INSTALL_PREFIX/lib/libg/${ESMF_STRING}/libesmf.dylib  $ESMF_DIR/lib/libg/${ESMF_STRING}/libesmf.dylib  
       install_name_tool -id $ESMF_INSTALL_PREFIX/lib/libg/${ESMF_STRING}/libesmf.dylib $ESMF_INSTALL_PREFIX/lib/libg/${ESMF_STRING}/libesmf.dylib  
        for F in $ESMF_INSTALL_PREFIX/bin/bing/${ESMF_STRING}/* ; do
          install_name_tool -change $ESMF_DIR/lib/libg/${ESMF_STRING}/libesmf.dylib $ESMF_INSTALL_PREFIX/lib/libg/${ESMF_STRING}/libesmf.dylib  $F
        done
    done
  done
done

