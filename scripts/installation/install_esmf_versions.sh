#!/bin/bash

TAGS=ESMF_7_1_0_beta_snapshot_51
export TAGS

export COMPS="gfortranclang" #"gfortranclang" # gfortranclang" # gfortran intel pgi gfortranclang pgigcc intelgcc
export COMMS="openmpi" #"openmpi" # openmpi" #"openmpi" #  mpiuni mpich2 intelmpi

test -n ${ESMF_DIR} || export ESMF_DIR = ${HOME}/devel/ESMF/esmf-code

if test -d ${ESMF_DIR} ; then
  echo "Using existing ESMF installation in $ESMF_DIR"
else
  echo "Using new ESMF installation in $ESMF_DIR"
  git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf $ESMF_DIR
fi

test -n ${ESMF_INSTALL_PREFIX} || export ESMF_INSTALL_PREFIX=/opt/esmf
mkdir -p ${ESMF_INSTALL_PREFIX}/etc

export ESMF_OS=$(bash ${ESMF_DIR}/scripts/esmf_os)
export ESMF_ABI=64

SED=sed

echo Using SED=${SED}
echo Using ESMF_OS=${ESMF_OS}
echo Using ESMF_INSTALL_PREFIX=${ESMF_INSTALL_PREFIX}
echo Using ESMF_DIR=${ESMF_DIR}
echo Installing for compilers ${COMMS}
echo Installing for communicators ${COMPS}

#echo y        | module clear

cd ${ESMF_DIR}

git stash && git stash drop
git pull origin master

for C in $COMMS ; do
  echo "Iterating for Communicator $C ============================================="
  ESMF_COMM=$C

  for G in $COMPS; do
    echo "Iterating for Compiler $G ============================================="
    ESMF_COMPILER=$G

    ESMF_NETCDF=split
    ESMF_NETCDF_INCLUDE=$(nc-config --includedir)
    ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib

    if [ $(hostname) = ocean-fe.fzg.local ]; then
      ESMF_NETCDF_INCLUDE=/opt/netcdf/3.6.2/${G}/include
      ESMF_NETCDF=standard

#     elif [ $G = intel ]; then
#       source /opt/intel/bin/ifortvars.sh intel64
#       source /opt/intel/bin/iccvars.sh intel64
#
#       export MPI_PATH=/opt/intel/mpich3
#       export PATH=$MPI_PATH/bin:$PATH
#       NETCDF_PATH=/opt/intel/netcdf4
#       ESMF_NETCDF_INCLUDE=${NETCDF_PATH}/include
    else
      ESMF_NETCDF_INCLUDE=$(nc-config --includedir)
    fi

    ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib

#    echo y  | module clear
#    module load ${ESMF_COMPILER} || continue
#    module load openmpi_ib || continue
#    module load netcdf/3.6.2 || continue
#    module list

    for T in $TAGS; do
       echo "Iterating for Tag $T ============================================="
       ESMF_SITE=$T
       ESMF_STRING=${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}
       git stash && git stash drop
       git checkout  -f $T


       rm -f ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}
       cp -r ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.default ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}

       # Fix -lmpi_f77 on recent Darwin/MacPorts
       if [[ ${ESMF_OS} == Darwin ]] ; then
         sed 's#-lmpi_f77##g' ${ESMF_DIR}/build_config/Darwin.gfortran.default/build_rules.mk  > ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}/build_rules.mk
       fi

       if [[ ${ESMF_COMPILER} == intel ]]; then
         sed 's#-openmp#-qopenmp#g' ${ESMF_DIR}/build_config/${ESMF_OS}.intel.default/build_rules.mk  > ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}/build_rules.mk
       fi

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
export ESMF_NETCDF=${ESMF_NETCDF}
export ESMF_NETCDF_INCLUDE=${ESMF_NETCDF_INCLUDE}
export ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_LIBPATH}
export ESMF_F90COMPILEOPTS=-DESMF_NO_SEQUENCE
unset ESMF_PIO
export ESMF_SITE=$T
export ESMF_COMPILER=$G
export ESMF_COMM=$C
export ESMFMKFILE=$ESMF_INSTALL_PREFIX/lib/libg/${ESMF_STRING}/esmf.mk
EOT

       echo "export ESMF_XERCES=standard" >> $HOME/.esmf_${ESMF_STRING}
       echo "# Comment the following line if you have libxerces"
       echo "unset ESMF_XERCES" >> $HOME/.esmf_${ESMF_STRING}

       source $HOME/.esmf_${ESMF_STRING}
       cat $HOME/.esmf_${ESMF_STRING}
       echo $PATH

       #test -f $ESMFMKFILE || (make distclean && make -j12 lib && make install)
       (make distclean && make -j12 lib && make install)

       test -f $ESMFMKFILE || continue
       test -f ${ESMF_INSTALL_PREFIX}/lib/libg/${ESMF_STRING}/libesmf.a || continue
       mkdir -p $ESMF_INSTALL_PREFIX/etc
       mv $HOME/.esmf_${ESMF_STRING} $ESMF_INSTALL_PREFIX/etc/${ESMF_STRING}
       make info > $ESMF_INSTALL_PREFIX/etc/${ESMF_STRING}.info

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
