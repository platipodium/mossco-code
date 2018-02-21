#!/bin/bash -x
# This script is part of MOSSCO.  It helps in installing ESMF
#
# @copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

# The script honours the following environment variables, and chooses
# default values if these are not set
# - ESMF_CONFIG_DIR
# - ESMF_TAGS
# - ESMF_COMPILERS
# - ESMF_COMMUNICATORS
# - ESMF_INSTALL_PREFIX and ESMF_DIR

# Determine where to save the ESMF configuration.  Default is $HOME
if [ -z ${ESMF_CONFIG_DIR} ]; then
  CONFIG_DIR=${HOME}
else
  CONFIG_DIR=${ESMF_CONFIG_DIR}
fi
test -d ${CONFIG_DIR} || mkdir -p ${CONFIG_DIR}
echo "  $0 saves ESMF configuration in ${CONFIG_DIR}"

test -z ${ESMF_DIR} && ESMF_DIR=${CONFIG_DIR}/devel/esmf-code
echo "  $0 uses ESMF sources in ${ESMF_DIR}"

# Determine what git tags of the ESMF repo are used.  Prefer the most
# recent one (we need >= 7_1_0_beta_snapshot_52 for regridding with extrapolation)
if [ -z "${ESMF_TAGS}" ]; then
  TAGS=ESMF_7_1_0_beta_snapshot_52
else
  TAGS=${ESMF_TAGS}
fi
echo "  $0 installs ESMF for tags ${TAGS}"

# Determine what compilers  are used.  We often test gfortran, gfortranclang,
# and intel
if [ -z "${ESMF_COMPILERS}" ]; then
  COMPS=gfortranclang # gfortran intel pgi gfortranclang pgigcc intelgcc
else
  COMPS="${ESMF_COMPILERS}"
fi
echo "  $0 installs ESMF for compilers ${COMPS}"

# Determine what communicators  are used.  We often test openmpi and intelmpi
if [ -z "${ESMF_COMMUNICATORS}" ]; then
  COMMS=openmpi # openmpi mpiuni mpich2 intelmpi
else
  COMMS="${ESMF_COMMUNICATORS}"
fi
echo "  $0 installs ESMF for communicators ${COMPS}"

# Determine what ESMF installation directory
if [ -z "${ESMF_INSTALL_PREFIX}" ]; then
  ESMF_INSTALL_PREFIX=${CONFIG_DIR}/opt
fi
mkdir -p ${ESMF_INSTALL_PREFIX}/etc
echo "  $0 installs ESMF to ${ESMF_INSTALL_PREFIX}"

if test -d ${ESMF_DIR} ; then
  echo "  $0 uses existing ESMF installation in $ESMF_DIR"
else
  echo " $0 pulls new ESMF installation in $ESMF_DIR"
  git clone --depth=1 git://esmf.git.sourceforge.net/gitroot/esmf/esmf ${ESMF_DIR}
fi

export ESMF_OS=$(uname -s)
export ESMF_ABI=64

echo "  $0 uses  ESMF_OS=${ESMF_OS} and ESMF_ABI=${ESMF_ABI}"

SED=sed
test -z $(which sed) || SED=$(which sed)
test -z $(which gsed) || SED=$(which gsed)

echo "  $0 uses  SED=${SED}"

cd ${ESMF_DIR}

git stash && git stash drop
git pull origin master

for C in $COMMS ; do
  echo "  $0 iterates for communicator $C "
  ESMF_COMM=$C

  for G in $COMPS; do
    echo "  $0 iterates for Compiler $G "
    ESMF_COMPILER=$G
    ESMF_NETCDF=split
    ESMF_NETCDF_INCLUDE=/usr/include

    if ! [ -z $(which nc-config) ]; then
      ESMF_NETCDF_INCLUDE=$(bash nc-config --includedir)
    fi

    ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib

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
      echo "  $0 uses netcdf include ${ESMF_NETCDF_INCLUDE}"
    else
      continue
    fi

    if test -d ${ESMF_NETCDF_LIBPATH} ; then
      echo "  $0 uses netcdf libdir ${ESMF_NETCDF_LIBPATH}"
    else
      continue
    fi

    for T in $TAGS; do
       echo "  $0 iterates for Tag $T "
       ESMF_SITE=$T
       ESMF_STRING=${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}

       ESMFMKFILE=${ESMF_INSTALL_PREFIX}/lib/libg/$ESMF_STRING/esmf.mk
       echo "  $0 uses esmf.mk ${ESMFMKFILE}"

       if test -r ${ESMFMKFILE}; then
         echo "  $0 found esmf.mk at location ${ESMFMKFILE}"
         continue
       fi

       git stash && git stash drop
       git checkout  -f $T

       rm -rf ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}
       cp -r ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.default ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}

       # Fix -lmpi_f77 on recent Darwin/MacPorts
       if [[ ${ESMF_OS} == Darwin ]] ; then
         sed 's#-lmpi_f77##g' ${ESMF_DIR}/build_config/Darwin.gfortran.default/build_rules.mk  > ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}/build_rules.mk
       fi

       if [[ ${ESMF_COMPILER} == intel ]]; then
         sed 's#-openmp#-qopenmp#g' ${ESMF_DIR}/build_config/${ESMF_OS}.intel.default/build_rules.mk  > ${ESMF_DIR}/build_config/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_SITE}/build_rules.mk
       fi

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

      #test -f $ESMFMKFILE || (make distclean && make -j12 lib && make install)
      (make distclean && make -j12 lib && make install)

      echo   cp $CONFIG_DIR/.esmf_${ESMF_STRING} $ESMF_INSTALL_PREFIX/etc/${ESMF_STRING}
      cp $CONFIG_DIR/.esmf_${ESMF_STRING} $ESMF_INSTALL_PREFIX/etc/${ESMF_STRING}
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
