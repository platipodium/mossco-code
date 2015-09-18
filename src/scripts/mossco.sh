#!/bin/bash

# @brief A MOSSCO startup script to facilitate compiling and running
#        You may want to link this script into directory within your $PATH
#
# This computer program is part of MOSSCO.
# @copyright Copyright (C) 2014, 2015, Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

# Initialize variables, set options with default values
OPTIND=1           # Reset in case getopts has been used previously in the shell.
GENERIC=1          # By default, use a hardcoded example
REMAKE=0           # Do not recompile if not necessary
BUILD_ONLY=0       # Executed, don't stop after build
DEFAULT=getm--fabm_pelagic--fabm_sediment--river--porosity--restart  # Default example
AUTOTITLE=1          # Whether to change the simulation title in mossco_run and getm.inp/gotmrun.nml
POSTPROCESS=NONE
NP=NONE
LOGLEVEL='undefined'
WAITTIME=0

# Function for printing usage of this script
function usage {
  echo
	echo "Usage: $0 [options] [example]"
	echo
	echo "Accepted options are -r, -b, -t <title>, -n <numproc>, -s <system> -l <loglevel> <example>"
	echo "If not provided, the default <example> is ${DEFAULT}"
	echo
	echo "    [-r] :  Rebuilds the [generic] example and MOSSCO coupled system"
	echo "    [-b] :  build-only.  Does not execute the example"
	echo "    [-t] :  give a title in mossco_run.nml and getm.inp/gotmrun.nml"
	echo "    [-p] :  specify the name of a postprocess script (only SLURM)"
	echo "            the default is <system>_postprocess.h"
	echo "    [-l A|W|E|N|T|D] :  specify the log level, as one of all|warning|error"
	echo "            |none|trace|default, if not specified, it is taken from mossco_run.nml."
	echo "    [-n X]: build for or/and run on X processors.  Default is content of par_setup.dat or n=1"
  echo
	echo "      [-n 0]:   MPI is not used at all."
  echo "      [-n XxY]: The layout X cpu-per-node times Y nodes is used"
  echo
	echo "    [-s M|S|J|F|B]: exeute batch queue for a specific system, which is"
  echo "            autodetected by default"
	echo
	echo "      [-s M]: MOAB system, writes moab.sh"
	echo "      [-s S]: SGE system, e.g. ocean.hzg.de, writes sge.sh"
	echo "      [-s J]: Slurm system, e.g. Jureca, Mistral, writes slurm.sh"
	echo "      [-s F]: Command line interactive, running in foreground"
	echo "      [-s B]: Command line interactive, running in background"
	echo
  echo "    [-w W] : wait W seconds for polling batch jobs (only -s J|B)"
	exit
}

# Function for selecting the queue on SGE system
function select_sge_queue {
  QSMALL=$(qstat -g c |grep small.q | awk '{print $5}')
  if [[ ${QSMALL} -ge  $1 ]] ; then
    echo small.q
  else
    echo all.q
  fi
}

# Function for predicting simulation time (adjusted for slurm)
# Calculation assumes 1 day per core and cpu-hour
function predict_time {
  NP=$1
  START=$(cat mossco_run.nml | grep start| awk -F"'" '{print $2}' | awk -F" " '{print $1}')
  STOP=$(cat mossco_run.nml | grep stop| awk -F"'" '{print $2}' | awk -F" " '{print $1}')
  Y1=$(echo ${START} | cut -d"-" -f1)
  Y2=$(echo ${STOP}  | cut -d"-" -f1)
  M1=$(echo ${START} | cut -d"-" -f2)
  M2=$(echo ${STOP}  | cut -d"-" -f2)
  D1=$(echo ${START} | cut -d"-" -f3)
  D2=$(echo ${STOP}  | cut -d"-" -f3)
  if [[ "x$D1" == "x" ]]; then
    echo "Check your input file, make sure it uses apostrophes around dates"
    exit
  fi
  D=$(expr \( ${Y2} - ${Y1} \) \* 365 + \( ${M2} - ${M1} \) \* 31 + ${D2} - ${D1} + 1)
  M=$(expr $D \* 40 / ${NP})
  H=$(expr $M / 60)
  M=$(expr $M % 60)
  echo  $H:$M:00
}

# Getopts parsing of command line arguments
while getopts ":rt:bn:s:l:w:" opt; do
  case "$opt" in
  r)  REMAKE=1
      ;;
  g)  GENERIC=1
      ;;
  b)  BUILD_ONLY=1
      ;;
  p)  POSTPROCESS=${OPTARG}
      ;;
  n)  NP=${OPTARG}
      ;;
  t)  TITLE=${OPTARG}
      AUTOTITLE=0
      ;;
  s)  SYSTEM=${OPTARG}
      ;;
  l)  LOGLEVEL=${OPTARG}
      ;;
  w)  WAITTIME=${OPTARG}
      ;;
  \?) usage
      ;;
  esac
done

shift $((OPTIND-1))

# Give default argument is none is provided
if [[ "x${1}" == "x" ]]; then ARG=${DEFAULT} ; else ARG=${1}; fi

if [[ "x${MOSSCO_DIR}" == "x" ]]; then
  echo "This script requires the environment variable MOSSCO_DIR."
  exit 1
fi

if ! test -d ${MOSSCO_DIR}; then
  echo "Your \$MOSSCO_DIR=$MOSSCO_DIR is not a directory"
  exit 1
fi

if [[ "x${ESMFMKFILE}" == "x" ]]; then
  echo "This script requires ESMF, pointed to by the environment variable ESMFMKFILE."
  exit 1
fi


if [[ ${GENERIC} == 1 ]] ; then
  DIR=${MOSSCO_DIR}/examples/generic
else
  DIR=${MOSSCO_DIR}/examples/${ARG}
fi

if ! test -d ${DIR} ; then
  echo
  if [[ ${GENERIC} == 0 ]] ; then
    echo "ERROR:  \"${ARG}\" is not a valid hardcoded example, ${DIR} does not exist."
  else
    echo "ERROR:	${DIR} does not exist. Check your MOSSCO installation."
  fi
  usage
fi

EXE=${DIR}/${ARG}
EXE=${EXE%%.yaml}
OWD=$(pwd)
SETUP=${OWD##*/}


if [[ ${GENERIC} == 1 ]] ; then
  if [[ ${REMAKE} == 0 ]] ; then
    test -x  ${EXE} || REMAKE=1
  fi
  if  [[ ${REMAKE} == 1 ]] ; then
    if ! test -f ${DIR}/create_coupling.py ; then
      echo
      echo "ERROR: Script create_coupling.py does not exist, check your MOSSCO installation."
      exit 1
    fi
    if ! test -x ${DIR}/create_coupling.py ; then
      echo
      echo "ERROR: Script create_coupling.py is not executable, please chmod +x this file."
      exit 1
    fi

    if test -f ${ARG}; then
      echo "Using local file ${ARG} as coupling specification."
      if [[ $(dirname ${ARG}) == "." ]] ; then
        ARG=$(pwd)/$(basename ${ARG})
      fi
    elif test -f ${ARG}.yaml; then
      echo "Using local file ${ARG}.yaml as coupling specification."
      if [[ $(dirname ${ARG}) == "." ]] ; then
        ARG=$(pwd)/$(basename ${ARG})
      fi
    elif test -f ${DIR}/${ARG} ; then
      echo "Using generic file ${ARG} as coupling specification."
    elif test -f ${DIR}/${ARG}.yaml ; then
      echo "Using generic file ${ARG}.yaml as coupling specification."
    else
      echo
      echo "ERROR: coupling spec ${ARG} or ${DIR}/${ARG}.yaml does not exist"
      echo
      exit 1
    fi
    cd ${DIR};
    python create_coupling.py ${ARG} || exit 1
    cd ${OWD}

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

if ! test -x  ${EXE} ; then
  echo
  echo "ERROR: Could not create executable ${EXE}"
  exit 1
fi

# Automatically determine system
if [[ $(which qstat 2> /dev/null) != "" ]] ; then AUTOSYSTEM=SGE
elif [[ $(which sstat 2> /dev/null) != "" ]] ; then AUTOSYSTEM=SLURM
elif [[ $(which msub 2> /dev/null) != "" ]] ; then AUTOSYSTEM=MOAB
fi

# Convert different system names
case ${SYSTEM} in
  moab|MOAB|M)  SYSTEM=MOAB
                ;;
  sge|SGE|S)    SYSTEM=SGE
                ;;
  J|SLURM|slurm)  SYSTEM=SLURM
                ;;
  F|FOREGROUND|fg)  SYSTEM=FOREGROUND
                ;;
  B|BACKGROUND|bg)  SYSTEM=BACKGROUND
                ;;
esac

if [[ ${AUTOSYSTEM}  = ${SYSTEM} ]] ; then :
elif [[ ${SYSTEM} = "" ]] ; then SYSTEM=${AUTOSYSTEM}
elif [[ ${AUTOSYSTEM} != "" ]] ; then echo "Overriding system ${AUTOSYSTEM} with ${SYSTEM}"
fi

if [[ ${SYSTEM} = "" ]] ; then SYSTEM=BACKGROUND; fi

# Adapt to different MPI implementations
case ${SYSTEM} in
  MOAB)  MPI_PREFIX="mpiexec"
                SYSTEM=MOAB
                ;;
  SGE)    MPI_PREFIX="mpirun"
                SYSTEM=SGE
                ;;
  SLURM)  MPI_PREFIX="srun"
                  SYSTEM=SLURM
                ;;
  *)  MPI_PREFIX="mpirun"
                ;;
esac


# Figure out default NP (1), or special settings if found in par_setup.dat
if [[ ${NP} == NONE ]]; then
  if test -f par_setup.dat ; then
    NP=$(head -n 1 par_setup.dat)
  else
    NP=1
  fi
fi


if [[ ${NP} == 0 ]]; then
  NP=1
  MPI_PREFIX=""
fi

NODES=1
PPN=$(echo ${NP} | cut -d'x' -f1)

if [[ "${PPN}" ==  "${NP}" ]]; then
  case ${SYSTEM} in
    MOAB)  NODES=$(expr \( $NP - 1 \) / 8 + 1 )
           PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
           NP=$(expr $NODES \* $PPN )
           ;;
    SLURM)  NODES=$(expr \( $NP - 1 \) / 48 + 1 )
           PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
           #NP=$(expr $NODES \* $PPN )
           if [[ ${POSTPROCESS} -eq NONE ]]; then
             POSTPROCESS=slurm_postprocess.sh
           fi
           ;;
    *)     ;;
  esac
else
  NODES=$(echo ${NP} | cut -d'x' -f2)
  TPP=$(echo ${NP} | cut -d'x' -f3)
  NP=$(expr ${NODES} \* ${PPN})
fi

echo "Building scripts for system ${SYSTEM} with MPI_PREFIX ${MPI_PREFIX} -np ${NP}"

case ${SYSTEM} in
  SLURM) if [[ ${POSTPROCESS} -eq NONE ]]; then
           POSTPROCESS=slurm_postprocess.sh
         fi
         ;;
  *)     ;;
esac

if [[ AUTOTITLE -eq 1 ]]; then
  TITLE=${SETUP}-${NODES}x${PPN}-$(basename ${ARG})
fi

RETITLE=1
if [[ ${TITLE} == 0 ]] ; then
  RETITLE=0
  TITTLE=${SETUP}
fi

if [[ "x${TITLE}" == "x" ]] ; then
  TITTLE=${SETUP}
fi


STDERR=${TITLE}.stderr
STDOUT=${TITLE}.stdout

if [[ "x${MPI_PREFIX}" != "x" ]] ; then
  case ${SYSTEM} in
    SGE)  MPI_PREFIX=${MPI_PREFIX}
        ;;
    SLURM)  MPI_PREFIX=${MPI_PREFIX}
        ;;
    *) MPI_PREFIX="${MPI_PREFIX} -np ${NP}"
       ;;
  esac
fi

EMAIL=${MOSSCO_USER_EMAIL:-$(who am i |cut -f1 -d" ")@$(hostname)}
WALLTIME=$(predict_time $NP)

case ${SYSTEM} in
  SLURM) cat << EOT > slurm.sh
#!/bin/bash -x

###SBATCH --account=${USER}
#SBATCH --ntasks=${NP}
#SBATCH --ntasks-per-core=1
#SBATCH --nodes=${NODES}
#SBATCH --tasks-per-node=${PPN}
#SBATCH --output=${TITLE}-%j.stdout
#SBATCH --error=${TITLE}-%j.stderr
#SBATCH --time=${WALLTIME}
#SBATCH --partition=batch
#SBATCH --mail-user=${EMAIL}
#SBATCH --mail-type=ALL
#SBATCH --job-name=${TITLE}

#export OMP_NUM_THREADS=56

${MPI_PREFIX} ${EXE}
EOT
;;
  MOAB) cat << EOT > moab.sh
#!/bin/bash -x

#MSUB -l nodes=${NODES}:ppn=${PPN}
#MSUB -l walltime=${WALLTIME}

#MSUB -N ${TITLE}

#MSUB -M ${EMAIL}
#MSUB -m abe

# Go to the current working directory (from where you submitted the job
cd \$PBS_O_WORKDIR
cat \$PBS_NODEFILE > \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.nodes
echo \$PBS_O_QUEUE >> \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.nodes
echo \$PBS_NUMPPN >> \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.nodes
echo \$PBS_JOBNAME >> \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.nodes
echo \$PBS_JOBID >> \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.nodes
cat moab.sh >> \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.nodes

${MPI_PREFIX} ${EXE} > \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.stdout 2> \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.stderr
EOT
;;
  SGE) cat << EOT > sge.sh
#!/bin/bash

#$ -N ${TITLE}
#$ -pe orte $NP
#$ -m beas
#$ -M ${EMAIL}
#$ -cwd
#$ -V

cat \$PE_HOSTFILE

#mkdir -p ${OUTDIR}
#test -d ${OUTDIR} || (echo "Directory ${OUTDIR} could not be created" ; exit 1)

${MPI_PREFIX} ${EXE} > ${STDOUT} 2> ${STDERR}
EOT
  ;;
esac

rm -rf PET?.${TITLE} ${TITLE}*stdout ${TITLE}*stderr ${STDERR} ${STDOUT}

# Unify loglevel input
case ${LOGLEVEL} in
  A|a|all|ALL) LOGLEVEL=all
     ;;
  W|w|warning|WARNING) LOGLEVEL=warning
     ;;
  E|e|error|ERROR) LOGLEVEL=error
     ;;
  T|t|trace|TRACE) LOGLEVEL=trace
     ;;
  N|n|none|NONE) LOGLEVEL=none
     ;;
  D|d|default|DEFAULT) LOGLEVEL=default
     ;;
  undefined)
     ;;
  *) echo "Loglevel ${LOGLEVEL} not defined in $0"
     echo "valid values are A|W|E|T|N|D"
     exit 1
     ;;
esac


SED=${SED:-$(which gsed 2> /dev/null )}
SED=${SED:-$(which sed 2> /dev/null )}

if test -f mossco_run.nml ; then
  if [[ "${LOGLEVEL}" != "undefined" ]] ; then
    ${SED} -i 's/loglevel =.*/loglevel = "'${LOGLEVEL}'",/' mossco_run.nml
  fi
fi


if [[ ${RETITLE} != 0 ]] ; then

  if ! test -x ${SED}; then
    echo
    echo "ERROR: Cannot execute the sed program $SED"
    exit 1
  fi

  if test -f getm.inp ; then
    ${SED} -i 's/runid =.*/runid = "'${TITLE}'",/' getm.inp
  fi

  if test -f gotmrun.nml ; then
    ${SED} -i 's/title =.*/title = "'${TITLE}'",/' gotmrun.nml
    ${SED} -i 's/out_fn =.*/out_fn = "'${TITLE}'_gotm",/' gotmrun.nml
  fi

  if test -f mossco_run.nml ; then
    ${SED} -i 's/title =.*/title = "'${TITLE}'",/' mossco_run.nml
  fi
fi

if test -f ./par_setup.dat ; then
  if test -f ./Parallel/par_setup.${NP}p.dat ; then
    ln -sf ./Parallel/par_setup.${NP}p.dat par_setup.dat
    echo "Linked Parallel/par_setup.${NP}p.dat to par_setup.dat"
  else
    echo "Warning: check that par_setup.dat is correctly setup for ${NP} processors"
  fi
fi

for F in $(ls *.dim 2> /dev/null) ; do
  if test -f Parallel/${F%%.dim}.${NP}p.dim ; then
    ln -sf Parallel/${F%%.dim}.${NP}p.dim $F
    echo "Linked Parallel/${F%%.dim}.${NP}p.dim to $F"
  else
    echo "Warning: check that $F is correctly setup for ${NP} processors"
  fi
done

if ! test -f mossco_run.nml ; then
  echo
  echo "ERROR: Need file mossco_run.nml to run"
  exit 1
fi

if [[ ${BUILD_ONLY} == 1 ]] ; then
  exit 0
fi

case ${SYSTEM} in
  MOAB)  if test $(which msub 2> /dev/null)  ; then
           msub moab.sh
           echo "Job ${TITLE} submitted for system ${SYSTEM}"
         else cat moab.sh ; fi
         ;;
  SGE)   if test $(which qsub 2> /dev/null) ; then
           QUEUE=$(select_sge_queue ${NP})
           qsub -q ${QUEUE} sge.sh
           echo "Job ${TITLE} submitted to queue ${QUEUE} for system ${SYSTEM}"
           qstat -g c
           qstat
         else
           cat sge.sh
         fi
         ;;
  SLURM) if test $(which sbatch 2> /dev/null) ; then
           JOBID=$(sbatch --parsable slurm.sh)
           echo "Job ${TITLE} with jobid ${JOBID} submitted to default queue for system ${SYSTEM}"
           if test -f ${POSTPROCESS}; then
             JOBID=$(sbatch --parsable --dependency=after:${JOBID} ${POSTPROCESS})
             echo "Postprocess job with jobid ${JOBID} submitted to default queue for system ${SYSTEM}"
           fi
           squeue -j ${JOBID}
           if [[ ${WAITTIME} -gt 0 ]]; then
             while true; do
               if ! squeue -j ${JOBID} &> /dev/null; then
                 break;
               fi
               if [[ "$(squeue -j ${JOBID} -h)" == "" ]]; then
                 break;
               fi
               echo "Waiting ${WAITTIME} seconds to poll job ${JOBID}"
               sleep ${WAITTIME}
             done
           fi
         else cat slurm.sh ; fi
         ;;
  BACKGROUND)  ${MPI_PREFIX} ${EXE}  1>  ${STDOUT}  2> ${STDERR} &
         PID=$!
         echo "${MPI_PREFIX} ${EXE}  " '1>'  "${STDOUT}"  ' 2> ' "${STDERR}" ' &'
         echo "Job ${TITLE} with PID ${PID} interactively running in background"
         if [[ ${WAITTIME} -gt 0 ]]; then
           echo "Waiting for process ${PID} to finish"
           wait $PID
         fi
         ;;
  FOREGROUND)  ${MPI_PREFIX} ${EXE}  1>  ${STDOUT}  2> ${STDERR}
         echo "${MPI_PREFIX} ${EXE}  " '1>'  "${STDOUT}"  ' 2> ' "${STDERR}"
         echo "Job ${TITLE} interactively running in foreground"
         ;;
  *)     echo "System ${SYSTEM} not defined in $0"; exit 1
         ;;
esac


test -f ${STDOUT} && tail -n 20 ${STDOUT}
test -f ${STDERR} && tail -n 20 ${STDERR}
test -f PET0.${TITLE} && tail -n 100 PET0.${TITLE}
