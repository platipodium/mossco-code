#!/bin/bash -x
# This script is part of MOSSCO.  It helps in installing ESMF
#
# @copyright (C) 2021-2022 Helmholtz-Zentrum Hereon
# @copyright (C) 2013-2021 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
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

test -z ${ESMF_DIR} && ESMF_DIR=${CONFIG_DIR}/devel/esmf/esmf-code
echo "  $0 uses ESMF sources in ${ESMF_DIR}"

# Determine what git tags of the ESMF repo are used.  Prefer the most
# recent one (we need >= 7_1_0_beta_snapshot_52 for regridding with extrapolation)
if [ -z "${ESMF_TAGS}" ]; then
  TAGS=v8.5.0
else
  TAGS=${ESMF_TAGS}
fi
echo "  $0 installs ESMF for tags ${TAGS}"

# Determine what compilers  are used.  We often test gfortran, gfortranclang,
# and intel
if [ -z "${ESMF_COMPILERS}" ]; then
  COMPS=gfortranclang # gfortranclang intel pgi gfortranclang pgigcc intelgcc
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
echo "  $0 installs ESMF for communicators ${COMMS}"

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
  git clone --depth=1 https://github.com/esmf-org/esmf.git  ${ESMF_DIR}
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
git pull

for C in $COMMS ; do
  echo "  $0 iterates for communicator $C "
  ESMF_COMM=$C

  for G in $COMPS; do
    echo "  $0 iterates for Compiler $G "
    ESMF_COMPILER=$G
    ESMF_NETCDF=split
    ESMF_NETCDF_INCLUDE=/usr/include
    ESMF_NETCDFF_INCLUDE=${ESMF_NETCDF_INCLUDE}

    if ! [ -z $(which nc-config) ]; then
      ESMF_NETCDF_INCLUDE=$(bash nc-config --includedir)
    fi
    ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_INCLUDE%%include}lib

    if test -d ${ESMF_NETCDF_INCLUDE} ; then
      echo "  $0 uses netcdf include ${ESMF_NETCDF_INCLUDE}"
    else
      echo "Could not find netcdf include, skipped installation"
      continue
    fi

    if test -d ${ESMF_NETCDF_LIBPATH} ; then
      echo "  $0 uses netcdf libdir ${ESMF_NETCDF_LIBPATH}"
    else
      echo "Could not find netcdf library path, skipped installation"
      continue
    fi

    if ! [ -z $(which nf-config) ]; then
       ESMF_NETCDFF_INCLUDE=$(bash nf-config --includedir)
    fi
    ESMF_NETCDFF_LIBPATH=${ESMF_NETCDFF_INCLUDE%%include}lib

    if test -d ${ESMF_NETCDFF_INCLUDE} ; then
      echo "  $0 uses netcdf-fortran include ${ESMF_NETCDFF_INCLUDE}"
      ESMF_NETCDF_INCLUDE=\"${ESMF_NETCDF_INCLUDE}" -I"${ESMF_NETCDFF_INCLUDE}\"
    else
      echo "Could not find netcdf-fortran include, skipped installation"
      continue
    fi

    if test -d ${ESMF_NETCDFF_LIBPATH} ; then
      echo "  $0 uses netcdf-fortran libdir ${ESMF_NETCDFF_LIBPATH}"
      ESMF_NETCDF_LIBPATH=\"${ESMF_NETCDF_LIBPATH}" -L"${ESMF_NETCDFF_LIBPATH}\"
    else
      echo "Could not find netcdf-fortran library path, skipped installation"
      continue
    fi

    for T in $TAGS; do
       echo "  $0 iterates for tag $T "
       ESMF_SITE=$T
       ESMF_STRING=${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}

       ESMFMKFILE=${ESMF_INSTALL_PREFIX}/lib/libg/$ESMF_STRING/esmf.mk
       echo "  $0 uses esmf.mk ${ESMFMKFILE}"

       if test -r ${ESMFMKFILE}; then
         echo "  $0 found esmf.mk at location ${ESMFMKFILE}. Skipped."
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
export ESMF_OS=${ESMF_OS}
export ESMF_ABI=${ESMF_ABI}
export ESMF_PTHREADS=OFF
export ESMF_OPENMP=ON
export ESMF_OPENACC=OFF
export ESMF_MOAB=internal # off
export ESMF_ARRAY_LITE=TRUE # exclude 5D, 6D, 7D interfaces
#export ESMF_NO_INTEGER_1_BYTE=TRUE
#export ESMF_NO_INTEGER_2_BYTE=TRUE
export ESMF_FORTRANSYMBOLS=default
export ESMF_MAPPER_BUILD=OFF
export ESMF_AUTO_LIB_BUILD=ON
export ESMF_DEFER_LIB_BUILD=ON
export ESMF_SHARED_LIB_BUILD=ON
export ESMF_BOPT=g
export ESMF_OPTLEVEL=3 # works only if BOPT=O
export ESMF_INSTALL_PREFIX=${ESMF_INSTALL_PREFIX}
export ESMF_LAPACK=internal
export ESMF_NETCDF=${ESMF_NETCDF}
export ESMF_NETCDF_INCLUDE=${ESMF_NETCDF_INCLUDE}
export ESMF_NETCDF_LIBPATH=${ESMF_NETCDF_LIBPATH}
#export ESMF_NETCDF_LIBS="-lnetcdff -lnetcdf"
#export ESMF_NETCDF=nc-config # only supported from ESMF8
export ESMF_F90COMPILEOPTS="${ESMF_F90COMPILEOPTS} -DESMF_NO_SEQUENCE"
export ESMF_SITE=$T
export ESMF_COMPILER=$G
export ESMF_COMM=$C
export ESMFMKFILE=$ESMF_INSTALL_PREFIX/lib/libg/${ESMF_STRING}/esmf.mk
export ESMF_LAPACK=system
export ESMF_LAPACK_LIBS=-lvecLibFort
#export ESMF_ACC_SOFTWARE_STACK=openmp4
export ESMF_XERCES=standard
# export ESMF_XERCES_INCLUDE=/opt/local/include
# export ESMF_XERCES_LIBS=-lxerces-c
# unset ESMF_XERCES # uncomment if not wanted
export ESMF_YAMLCPP=internal
export ESMF_PIO=internal
# unset ESMF_PIO # uncomment if not wanted
EOT

      source $CONFIG_DIR/.esmf_${ESMF_STRING}
      cat $CONFIG_DIR/.esmf_${ESMF_STRING}

      #test -f $ESMFMKFILE || (make distclean && make -j12 lib && make install)
      (make -s distclean && make -s -j12 lib && make -s install)

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
