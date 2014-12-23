#!/bin/bash

set -e

if [[ "x${1}" == "x-g" ]]; then 
  GENERIC=1
  REMAKE=0
elif [[ "x${1}" == "x-gr" ]]; then 
  GENERIC=1
  REMAKE=1
elif [[ "x${1}" == "x-rg" ]]; then 
  GENERIC=1
  REMAKE=1
elif [[ "x${1}" == "x-r" ]]; then 
  GENERIC=0
  REMAKE=1
fi

shift

# Give default argument is none is provided
if [[ "x${1}" == "x" ]]; then ARG=benthic_geoecology ; else ARG=${1}; fi

if [[ ${GENERIC} == 1 ]] ; then 
  DIR=${MOSSCO_DIR}/examples/generic
else
  DIR=${MOSSCO_DIR}/examples/${ARG}
fi

test -d ${DIR} || ( echo "ERROR, directory ${DIR} does not exist" ; exit 1)
EXE=${DIR}/${ARG}


if [[ ${GENERIC} == 1 ]] ; then
  if [[ ${REMAKE} == 0 ]] ; then
    test -x  ${EXE} || REMAKE=1
  fi
  if  [[ ${REMAKE} == 1 ]] ; then
    test -f ${DIR}/create_coupling.py || ( echo "ERROR, script create_coupling.py does not exist"; exit 1)
    test -f ${EXE}.yaml || ( echo "ERROR, coupling spec ${EXE}.yaml does not exist"; exit 1)
    (cd ${DIR}; python create_coupling.py ${ARG})
  fi
  make -C ${DIR}
  test -x  ${EXE} || ( echo "ERROR, could not create executable ${EXE}" ; exit 1)
else
  if [[ ${REMAKE} == 0 ]] ; then
    test -x  ${DIR}/${ARG} || REMAKE=1
  fi
  if  [[ ${REMAKE} == 1 ]] ; then
    make -C ${DIR}
  fi
  test -x  ${DIR}/${ARG} || ( echo "ERROR, could not create executable ${EXE}" ; exit 1)
fi

test -f mossco_run.nml || (echo "ERROR, need file mossco_run.nml to run" ; exit 1)
TITLE=$(cat mossco_run.nml | grep title | cut -f2 -d "'")

echo ${EXE}  ${TITLE}

rm -rf PET?.${TITLE} ${TITLE}.stdout
${EXE} | tee ${TITLE}.stdout
tail -n 300 PET0.${TITLE}
