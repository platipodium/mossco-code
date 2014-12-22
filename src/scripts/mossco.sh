#!/bin/bash

set -e

if [[ "x${1}" == "x" ]]; then ARG=benthic_geoecology ; else ARG=${1}; fi

DIR=${MOSSCO_DIR}/examples/${ARG}
test -d ${DIR} || exit 1

test -f mossco_run.nml || exit 1
TITLE=$(cat mossco_run.nml | grep title | cut -f2 -d "'")

make -C ${DIR}

EXE=${DIR}/${ARG}
echo ${EXE}  ${TITLE}
test -x ${EXE} || exit 1

rm -rf PET?.${TITLE} ${TITLE}.stdout
${EXE} | tee ${TITLE}.stdout
tail -n 300 PET0.${TITLE}
