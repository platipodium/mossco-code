#!/bin/bash

# Uses getopts functionality, initialize variables
OPTIND=1         # Reset in case getopts has been used previously in the shell.
GENERIC=0
REMAKE=0
BUILD_ONLY=0
DEFAULT=benthic_geoecology


usage(){
	echo "Usage: $0 [options] [example]"
	echo "Accepted options are -r, -b, -g"
	echo "If not provided, the default example is ${DEFAULT}"
	exit 1
}


while getopts "rgb" opt; do
  case "$opt" in
  r)  REMAKE=1
      ;;
  g)  GENERIC=1
      ;;
  b)  BUILD_ONLY=1; REMAKE=1
      ;;
  \?) usage
      ;;
  esac
done

shift $((OPTIND-1))

# Give default argument is none is provided
if [[ "x${1}" == "x" ]]; then ARG=${DEFAULT} ; else ARG=${1}; fi

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
    rm -f ${EXE}
    make -C ${DIR}
  fi
else
  if [[ ${REMAKE} == 0 ]] ; then
    test -x  ${EXE} || REMAKE=1
  fi
  if  [[ ${REMAKE} == 1 ]] ; then
    rm -f ${EXE}
    make -C ${DIR}
  fi
fi

test -x  ${EXE} || ( echo "ERROR, could not create executable ${EXE}" ; exit 1)

if [[ ${BUILD_ONLY} == 1 ]] ; then
  exit 0
fi

test -f mossco_run.nml || (echo "ERROR, need file mossco_run.nml to run" ; exit 1)
TITLE=$(cat mossco_run.nml | grep title | cut -f2 -d "'")

echo ${EXE} ${TITLE}

STDERR=${ARG}-${TITLE}.stderr
STDOUT=${ARG}-${TITLE}.stdout

rm -rf PET?.${TITLE} ${TITLE}.stdout ${STDERR} ${STDOUT}
${EXE} 1>  ${STDOUT} 2> ${STDERR}

tail -n 20 ${STDOUT}
tail -n 20 ${STDERR}
tail -n 100 PET0.${TITLE}




