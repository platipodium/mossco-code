#!/bin/bash

# Uses getopts functionality, initialize variables
OPTIND=1         # Reset in case getopts has been used previously in the shell.
GENERIC=0
REMAKE=0
BUILD_ONLY=0
NP=1
DEFAULT=benthic_geoecology
SYSTEM=INTERACTIVE

usage(){
	echo "Usage: $0 [options] [example]"
	echo "Accepted options are -r, -b, -g, -n <numproc>, -s <system>"
	echo "If not provided, the default example is ${DEFAULT}"
	exit 1
}


while getopts "rgbn:s:" opt; do
  case "$opt" in
  r)  REMAKE=1
      ;;
  g)  GENERIC=1
      ;;
  b)  BUILD_ONLY=1; REMAKE=1
      ;;
  n)  NP=${OPTARG}
      ;;
  s)  SYSTEM=${OPTARG}
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

case ${SYSTEM} in
  moab|MOAB|M)  MPI_PREFIX="mpiexec"
                SYSTEM=MOAB
                ;;
  sge|SGE|S)    MPI_PREFIX="mpirun"
                SYSTEM=SGE
                ;;
  *)  MPI_PREFIX="mpirun "
                ;;
esac

if [[ ${NP} == 0 ]]; then
  MPI_PREFIX=""
  NP=1
fi

test -f mossco_run.nml || (echo "ERROR, need file mossco_run.nml to run" ; exit 1)
TITLE=$(cat mossco_run.nml | grep title | cut -f2 -d "'")

echo ${MPI_PREFIX} ${EXE} ${TITLE}

STDERR=${ARG}-${TITLE}.stderr
STDOUT=${ARG}-${TITLE}.stdout

NODES=1
PPN=${NP}

case ${SYSTEM} in
  MOAB)  NODES=$(expr \( $NP - 1 \) / 8 + 1 )
         PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
         NP=$(expr $NODES \* $PPN )
         ;;
  *)     ;;
esac

MPI_PREFIX="${MPI_PREFIX} -np ${NP}"

cat << EOT > moab.sh
#!/bin/bash -x

#MSUB -l nodes=${NODES}:ppn=${PPN}
#MSUB -l walltime=0:30:00

#MSUB -M carsten.lemmen@hzg.de
#MSUB -m abe
#MSUB -N ${TITLE}

# Go to the current working directory (from where you submitted the job
cd \$PBS_O_WORKDIR
cat \$PBS_NODEFILE  \$PBS_O_WORKDIR/\$PBSJOBID.nodes
echo $\PBS_O_QUEUE >> \$PBS_O_WORKDIR/\$PBSJOBID.nodes
echo $\PBS_NUMPPN >> \$PBS_O_WORKDIR/\$PBSJOBID.nodes
echo $\PBS_JOBNAME >> \$PBS_O_WORKDIR/\$PBSJOBID.nodes
echo $\PBS_JOBID >> \$PBS_O_WORKDIR/\$PBSJOBID.nodes

${MPI_PREFIX} ${EXE} > \$PBS_O_WORKDIR/\$PBSJOBID.stdout 2> \$PBS_O_WORKDIR/\$PBSJOBID.stderr
EOT

cat << EOT > sge.sh
#!/bin/bash

#$ -N ${TITLE}
#$ -pe orte $NP
#$ -cwd
#$ -V

cat \$PE_HOSTFILE

#mkdir -p ${OUTDIR}
#test -d ${OUTDIR} || (echo "Directory ${OUTDIR} could not be created" ; exit 1)

${MPI_PREFIX} ${EXE} > ${STDOUT} 2> ${STDERR}
EOT


rm -rf PET?.${TITLE} ${TITLE}.stdout ${STDERR} ${STDOUT}

if [[ ${BUILD_ONLY} == 1 ]] ; then
  exit 0
fi

case ${SYSTEM} in
  MOAB)  if test -x msub ; then msub moab.sh ; else cat moab.sh ; fi
         ;;
  SGE)   if test -x qsub ; then qsub sge.sh ; else cat sge.sh ; fi
         ;;
  *)  ${MPI_PREFIX} ${EXE}  1>  ${STDOUT}  2> ${STDERR}
         ;;
esac

test -f ${STDOUT} && tail -n 20 ${STDOUT}
test -f ${STDERR} && tail -n 20 ${STDERR}
test -f PET0.${TITLE} && tail -n 100 PET0.${TITLE}




