#!/bin/bash

export TAGS=ESMF_7_1_0_beta_snapshot_52
# The first optional argument is the base configuration dir, where the
# environment variable file .esmf_.... is stored, and where the ESMF_DIR
# is configured if not set

if [ "x$1" == "x" ]; then
  export CONFIG_DIR=${HOME}
else
  export CONFIG_DIR=${1}
fi

test -d ${CONFIG_DIR} || mkdir -p ${CONFIG_DIR}

# Environment variables COMPS, COMMS, ESMF_DIR and ESMF_INSTALL_PREFIX are honored
# or filled with default values here

test -n ${COMPS} || export COMPS="gfortranclang" #"gfortran intel pgi gfortranclang pgigcc intelgcc
test -n ${COMMS} || export COMMS="openmpi" #"openmpi" # openmpi" #"openmpi" #  mpiuni mpich2 intelmpi

test -n ${ESMF_DIR} || export ESMF_DIR = ${CONFIG_DIR}/devel/esmf-code

if test -d ${ESMF_DIR} ; then
  echo "Using existing ESMF installation in $ESMF_DIR"
else
  echo "Using new ESMF installation in $ESMF_DIR"
<<<<<<< HEAD
  git clone --depth=1 git://esmf.git.sourceforge.net/gitroot/esmf/esmf ${ESMF_DIR}
=======
  git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf $ESMF_DIR
>>>>>>> eb7a73091d2ed7c55d26d2e6a91c3a60d0e50bcf
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
echo Using compilers ${COMMS}
echo Using communicators ${COMPS}

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

    ESMF_NETCDF_INCLUDE=$(bash nc-config --includedir)
    ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib

    NCCONFIG=$(which nc-config)
    if [[ "x$NCCONFIG" == "x" ]] ; then
      ESMF_NETCDF_INCLUDE=/usr/include
      ESMF_NETCDF_LIBPATH=/usr/lib
    fi

    if [ $(hostname) = ocean-fe.fzg.local ]; then
      ESMF_NETCDF_INCLUDE=/opt/netcdf/3.6.2/${G}/include
      ESMF_NETCDF=standard
      ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib

#     elif [ $G = intel ]; then
#       source /opt/intel/bin/ifortvars.sh intel64
#       source /opt/intel/bin/iccvars.sh intel64
#
#       export MPI_PATH=/opt/intel/mpich3
#       export PATH=$MPI_PATH/bin:$PATH
#       NETCDF_PATH=/opt/intel/netcdf4
#       ESMF_NETCDF_INCLUDE=${NETCDF_PATH}/include
    fi

#    echo y  | module clear
#    module load ${ESMF_COMPILER} || continue
#    module load openmpi_ib || continue
#    module load netcdf/3.6.2 || continue
#    module list

    if test -d ${ESMF_NETCDF_INCLUDE} ; then
      echo using netcdf include ${ESMF_NETCDF_INCLUDE}
    else
      continue
    fi

    if test -d ${ESMF_NETCDF_LIBPATH} ; then
      echo using netcdf libdir ${ESMF_NETCDF_LIBPATH}
    else
      continue
    fi

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

cat << EOT > $CONFIG_DIR/.esmf_${ESMF_STRING}
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

       echo "export ESMF_XERCES=standard" >> $CONFIG_DIR/.esmf_${ESMF_STRING}
       echo "# Comment the following line if you have libxerces"
       echo "unset ESMF_XERCES" >> $CONFIG_DIR/.esmf_${ESMF_STRING}

       source $CONFIG_DIR/.esmf_${ESMF_STRING}
       cat $CONFIG_DIR/.esmf_${ESMF_STRING}
       echo $PATH

       #test -f $ESMFMKFILE || (make distclean && make -j12 lib && make install)
       (make distclean && make -j12 lib && make install)

       test -f $ESMFMKFILE || continue
       test -f ${ESMF_INSTALL_PREFIX}/lib/libg/${ESMF_STRING}/libesmf.a || continue
       mkdir -p $ESMF_INSTALL_PREFIX/etc
       mv $CONFIG_DIR/.esmf_${ESMF_STRING} $ESMF_INSTALL_PREFIX/etc/${ESMF_STRING}
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
