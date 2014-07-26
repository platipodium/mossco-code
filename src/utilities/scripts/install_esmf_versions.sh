#!/bin/bash

export TAGS="ESMF_7_0_0_beta_snapshot_14 ESMF_6_3_0rp1_beta_snapshot_10  ESMF_5_3_1_beta_snapshot_18"
#export TAGS="ESMF_4_0_1_beta_snapshot_01 ESMF_3_1_2_beta_snapshot_12"
COMPS="gfortran" # intel pgi"
COMMS="openmpi" #  mpiuni"

test -n ${ESMF_DIR} || export ESMF_DIR = ${HOME}/devel/ESMF/esmf-code
cd $ESMF_DIR && git pull

test -n ${ESMF_INSTALL_PREFIX} || export ESMF_INSTALL_PREFIX=/opt/esmf
mkdir -p ${ESMF_INSTALL_PREFIX}/etc

export ESMF_OS=$(${ESMF_DIR}/scripts/esmf_os)
export ESMF_ABI=64

#echo y        | module clear

for C in $COMMS ; do
  echo "Iterating for Communicator $C ============================================="
  ESMF_COMM=$C

  for G in $COMPS; do
    echo "Iterating for Compiler $G ============================================="
    ESMF_COMPILER=$G

    case $(hostname) in
    KSEZ8002) 
      LOCAL_PATH=/opt/gcc48
      ESMF_NETCDF_INCLUDE=${LOCAL_PATH}/include
      ESMF_NETCDF_LIBPATH=${LOCAL_PATH}/lib
      export PATH=${LOCAL_PATH}/bin:$PATH
      ;;
    *)
      ESMF_NETCDF_INCLUDE=$(nc-config --includedir)
      ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib
      ;;
    esac
    
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
       sed -i tmp 's#-lmpi_f77##g' ${ESMF_DIR}/build_config/Darwin.gfortran.default/build_rules.mk || continue
       
       ln -sf ${ESMF_DIR}/build_config/Darwin.gfortran.default ${ESMF_DIR}/build_config/Darwin.gfortran.${ESMF_SITE}
       
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

