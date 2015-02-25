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
NP=1               # Run on one processor
DEFAULT=getm--fabm_pelagic--fabm_sediment--netcdf  # Default example
SYSTEM=INTERACTIVE                  # Interactive shell as default system
AUTOTITLE=1          # Whether to change the simulation title in mossco_run and getm.inp

# Function for printing usage of this script
usage(){
  echo
	echo "Usage: $0 [options] [example]"
	echo
	echo "Accepted options are -r, -b, -t <title>, -n <numproc>, -s <system> <example>"
	echo "If not provided, the default <example> is ${DEFAULT}"
	echo
	echo "    [-r] :  Rebuilds the [generic] example and MOSSCO coupled system"
	echo "    [-b] :  build-only.  Does not execute the example"
	echo "    [-t] :  give a title in mossco_run.nml and getm.inp"
	echo "    [-n X]: build for or/and run on X processors.  If you set n=0, then
	echo "            MPI is not used at all. Default is n=1
	echo "    [-s M|S|J]: exeute batch queue for a specific system"
	echo
	echo "      [-s M]: MOAB system, e.g. juropa.fz-juelich.de, writes moab.sh"
	echo "      [-s S]: SGE system, e.g. ocean.hzg.de, writes sge.sh"
	echo "      [-s J]: Slurm system, e.g. juropatest, writes slurm.sh"
	echo
	exit
}

# Getopts parsing of command line arguments
while getopts ":rt:bn:s:" opt; do
  case "$opt" in
  r)  REMAKE=1
      ;;
  g)  GENERIC=1
      ;;
  b)  BUILD_ONLY=1
      ;;
  n)  NP=${OPTARG}
      ;;
  t)  TITLE=${OPTARG}
      AUTOTITLE=0
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

# Adapt to different MPI implementations
case ${SYSTEM} in
  moab|MOAB|M)  MPI_PREFIX="mpiexec"
                SYSTEM=MOAB
                ;;
  sge|SGE|S)    MPI_PREFIX="mpirun"
                SYSTEM=SGE
                ;;
  J|SLURM|slurm)  MPI_PREFIX="srun"
                  SYSTEM=SLURM
                ;;
  *)  MPI_PREFIX="mpirun"
                ;;
esac

if [[ ${NP} == 0 ]]; then
  MPI_PREFIX=""
  NP=1
fi

NODES=1
PPN=${NP}

case ${SYSTEM} in
  MOAB)  NODES=$(expr \( $NP - 1 \) / 8 + 1 )
         PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
         NP=$(expr $NODES \* $PPN )
         ;;
  SLURM)  NODES=$(expr \( $NP - 1 \) / 28 + 1 )
         PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
         NP=$(expr $NODES \* $PPN )
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

case ${SYSTEM} in
  SLURM) cat << EOT > slurm.sh
#!/bin/bash -x

#SBATCH --nodes=${NODES}
#SBATCH --tasks-per-node=${PPN}
#SBATCH --output=${TITLE}-%j.stdout
#SBATCH --error=${TITLE}-%j.stderr
#SBATCH --time=00:00:06
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
#MSUB -l walltime=0:12:00

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

if [[ RETITLE != 0 ]] ; then

  SED=${SED:-$(which gsed)} 2> /dev/null
  SED=${SED:-$(which sed)} 2> /dev/null

  if ! test -x ${SED}; then
    echo
    echo "ERROR: Cannot execute the sed program $SED"
    exit 1
  fi

  if test -f getm.inp ; then
    ${SED} -i 's/runid =.*/runid = "'${TITLE}'",/' getm.inp
  fi

  if test -f mossco_run.nml ; then
    ${SED} -i 's/title =.*/title = "'${TITLE}'",/' mossco_run.nml
  fi
fi

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
           if [[ $NP < 49 ]] ; then
             qsub -q.small sge.sh ;
           else
             qsub sge.sh
           fi
           echo "Job ${TITLE} submitted for system ${SYSTEM}"
           qstat -g c
           qstat
         else cat sge.sh ; fi
         ;;
  SLURM) if test $(which sbatch 2> /dev/null) ; then
           sbatch slurm.sh
           echo "Job ${TITLE} submitted for system ${SYSTEM}"
         else cat slurm.sh ; fi
         ;;
  INTERACTIVE)  ${MPI_PREFIX} ${EXE}  1>  ${STDOUT}  2> ${STDERR} &
         echo "Job ${TITLE} interactively running in background"

         ;;
  *)     echo "System not defined in $0"; exit 1
         ;;
esac


test -f ${STDOUT} && tail -n 20 ${STDOUT}
test -f ${STDERR} && tail -n 20 ${STDERR}
test -f PET0.${TITLE} && tail -n 100 PET0.${TITLE}




