#!/bin/bash

# @brief A MOSSCO startup script to facilitate compiling and running
#        You may want to link this script into directory within your $PATH
#
# This computer program is part of MOSSCO.
# @copyright Copyright (C) 2021-2023 Helmholtz-Zentrum hereon GmbH
# @copyright Copyright (C) 2014-2021 Helmholtz-Zentrum Geesthacht GmbH
# @author Carsten Lemmen, <carsten.lemmen@hereon>
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
COMPILE_ONLY=0
WALLTIME=00:00:00     # Default run time, if this is zero the run time is estimated automatically
DEFAULT=getm--fabm_pelagic--fabm_sediment--river--porosity--deposition  # Default example
AUTOTITLE=1          # Whether to change the simulation title in mossco_run and getm.inp/gotmrun.nml
POSTPROCESS=NONE
NP=NONE
LOGLEVEL='undefined'
WAITTIME=0
QUEUE='undefined'
RESTART=0
PYTHON=$(which python)

# Function for printing usage of this script
function usage {
  echo
	echo "Usage: $0 [options] [example]"
	echo
	echo "Common options are -r, -b, -t <title>, -n <numproc>, -s <system> -l <loglevel> <example>"
	echo "If not provided, the default <example> is ${DEFAULT}"
	echo
  echo "    [-b] :  build-only.  Does not execute the example. Rebuilds job script."
	echo "    [-c] :  compile-only.  Does not prepare and execute the example"
	echo "    [-l A|W|E|N|T|D] :  specify the log level, as one of all|warning|error"
	echo "            |none|trace|default, if not specified, it is taken from mossco_run.nml."
	echo "    [-n X[:YxZ]]: build for or/and run on X processors.  Default is content of par_setup.dat or n=1"
  echo
	echo "      [-n 0]:   MPI is not used at all."
  echo "      [-n X[:YxZ]]: The layout Y cpu-per-node times Z nodes is used"
  echo
  echo "    [-p] :  specify the name of a postprocess script (only SLURM)"
  echo "            the default is <system>_postprocess.h"
  echo "    [-q QUEUE] :  Selects a queue/partition with name QUEUE"
  echo "    [-r] :  Rebuilds the [generic] example and MOSSCO coupled system"
#  echo "    [-R X] :  Restart simulation X times, default is zero"
  echo "    [-s M|S|J|F|B|P]: exeute batch queue for a specific system, which is"
  echo "            autodetected by default"
  echo
  echo "      [-s P]: PBS system, writes pbs.sh"
  echo "      [-s M]: MOAB system, writes moab.sh"
  echo "      [-s S]: SGE system, e.g. ocean.hzg.de, writes sge.sh"
  echo "      [-s J]: Slurm system, e.g. Juwels, Mistral, Strand, writes slurm.sh"
  echo "      [-s F]: Command line interactive, running in foreground"
  echo "      [-s B]: Command line interactive, running in background"
  echo
  echo "    [-t] :    give a title in mossco_run.nml and getm.inp/gotmrun.nml"
  echo "    [-w W] :  wait W seconds for polling batch jobs (only -s J|B)"
  echo "    [-z HH:MM:SS] : set HH:MM:SS as maximum run duration walltime"
  echo "              of a job in the format HH:MM:SS as hours, minutes, seconds. If not"
  echo "              set, the time is estimated for your queuing system"
  exit
}

# Function for selecting the queue on SGE system
function select_sge_queue {
  QSMALL=$(qstat -g c |grep small.q | awk '{print $5}')
  #if [[ ${QSMALL} -ge  $1 ]] ; then
  #  echo small.q
  #else
    echo all.q
  #fi
}

# Function gives back the number of days
# between two times given as positional arguments
function timeInterval() {

  # Replace TZ markers with space to separate date from time
  timeB=${2//[A-z]/ }
  timeA=${1//[A-z]/ }

  #echo $timeB $timeA

  YMDA=$(echo ${START} | cut -d" " -f1)
  YMDB=$(echo ${STOP} | cut -d" " -f1)

  YA=$(echo ${YMDA} | cut -d"-" -f1)
  YB=$(echo ${YMDB}  | cut -d"-" -f1)
  MA=$(echo ${YMDA} | cut -d"-" -f2)
  MB=$(echo ${YMDB}  | cut -d"-" -f2)
  DA=$(echo ${YMDA} | cut -d"-" -f3)
  DB=$(echo ${YMDB}  | cut -d"-" -f3)
  if [[ "x$DA" == "x" ]]; then
    echo "Check your input file, make sure it uses apostrophes around dates"
    exit
  fi
  D=$(expr \( ${YB} - ${YA} \) \* 365 + \( ${MB} - ${MA} \) \* 31 + ${DB} - ${DA} + 1)
  echo  $D
}

# Function for predicting simulation time (adjusted for slurm)
# The positional parameters are NP, SYSTEM, RESTART
function predict_time() {

  NP=$1
  RESTART=1
  if [[ $3 -gt $RESTART ]] ; then RESTART=$3; fi

  S=30
  case ${2} in
    PBS)  S=1000;;
    SGE)  S=300;;
    SLURM) S=1000;; # estimated speedup on slurm
  esac

  START=$(cat ${NML} | grep -v --regexp ' *!'| grep start | awk -F"'" '{print $2}' | awk -F" " '{print $1}')
  STOP=$(cat ${NML} | grep -v --regexp ' *!'| grep stop | awk -F"'" '{print $2}' | awk -F" " '{print $1}')

  D=$(timeInterval "$START" "$STOP")
  M=$(expr $D \* 200000 / ${NP} / ${S} / ${RESTART} + 2)
  H=$(expr $M / 60)
  M=$(expr $M % 60)
  echo  $H:$M:00
}

# Getopts parsing of command line arguments
while getopts ":rt:bcn:s:l:w:p:q:z:R:" opt; do
  case "$opt" in
  r)  REMAKE=1
      ;;
  g)  GENERIC=1
      ;;
  b)  BUILD_ONLY=1
      ;;
  c)  COMPILE_ONLY=1
      ;;
  n)  NP=${OPTARG}
      ;;
  t)  TITLE=${OPTARG}
      AUTOTITLE=0
      ;;
  l)  LOGLEVEL=${OPTARG}
      ;;
  p)  POSTPROCESS=${OPTARG}
      ;;
  q)  QUEUE=${OPTARG}
      ;;
  R)  RESTART=${OPTARG}
      ;;
  s)  SYSTEM=${OPTARG}
      ;;
  w)  WAITTIME=${OPTARG}
      ;;
  z)  WALLTIME=${OPTARG}
      ;;
  \?) usage
      ;;
  esac
done

shift $((OPTIND-1))

# Give default argument is none is provided
if [[ "x${1}" == "x" ]]; then ARG=${DEFAULT} ; else ARG=${1}; fi
if [[ "x${2}" == "x" ]]; then NML=mossco_run.nml ; else NML=${2}; fi

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

EXE_BASE=$(basename ${ARG%%.yaml})
OWD=$(pwd)
SETUP=${OWD##*/}

if [[ ${REMAKE} == 0 ]] ; then
  if test -x ${EXE_BASE}; then
    EXE=./${EXE_BASE}
    echo "Using existing local ${EXE}"
  else
    EXE=${MOSSCO_DIR}/examples/${ARG}/${EXE_BASE}
    if test -x ${EXE}; then
      echo "Using existing ${EXE}"
    else
      REMAKE=1
      EXE=
    fi
  fi
fi

ARG=${ARG%%.yaml}

if  [[ ${REMAKE} == 1 ]] ; then
  # Clean up old executables
  if test -x ./${EXE_BASE}; then
    rm -f ./${EXE_BASE}
  fi
  if test -x ${MOSSCO_DIR}/examples/${ARG}/${EXE_BASE}; then
    rm -f ${MOSSCO_DIR}/examples/${ARG}/${EXE_BASE}
  fi

  if [[ ${GENERIC} == 1 ]] ; then
    DIR=${MOSSCO_DIR}/examples/generic

    if test -f ${ARG}.yaml; then
      echo "Using local file ${ARG}.yaml as coupling specification."
      if [[ $(dirname ${ARG}) == "." ]] ; then
        ARG=$(pwd)/$(basename ${ARG})
      fi
    elif test -f ${DIR}/${ARG}.yaml ; then
      echo "Using generic file ${ARG}.yaml as coupling specification."
      ARG=${DIR}/$(basename ${ARG})
    else
      echo "Coupling spec ${ARG}.yaml or ${DIR}/${ARG}.yaml does not exist"
      GENERIC=0
      DIR=${MOSSCO_DIR}/examples/${ARG}
      echo "Trying hardcoded example ${DIR}"
      if ! test -d ${DIR} ; then
        echo
        echo "ERROR:  \"${ARG}\" is not a valid hardcoded example, ${DIR} does not exist."
        usage
        exit 1
      fi
      EXE=${DIR}/${EXE}
    fi

    if [[ ${GENERIC} == 1 ]]; then
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
      ${PYTHON} ${DIR}/create_coupling.py ${ARG} || exit 1
      EXE=./${EXE_BASE}
    fi

    make -C ${DIR}

  fi
fi

if ! test -x  ${EXE} ; then
  echo
  echo "ERROR: Could not create executable ${EXE}"
  exit 1
fi

if [[ ${COMPILE_ONLY} == 1 ]] ; then
  exit 0
fi

# Automatically determine system
if [[ $(hostname) == service0 ]] ; then AUTOSYSTEM=PBS
elif [[ $(which qstat 2> /dev/null) != "" ]] ; then AUTOSYSTEM=SGE
elif [[ $(which sstat 2> /dev/null) != "" ]] ; then AUTOSYSTEM=SLURM
elif [[ $(which msub 2> /dev/null) != "" ]] ; then AUTOSYSTEM=MOAB
fi

# Convert different system names
case ${SYSTEM} in
  moab|MOAB|M)  SYSTEM=MOAB
                ;;
  sge|SGE|S)    SYSTEM=SGE
                ;;
  pbs|PBS|P)    SYSTEM=PBS
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
  PBS)  MPI_PREFIX="mpiexec_mpt"
                ;;
  MOAB)  MPI_PREFIX="mpiexec"
                ;;
  SGE)    MPI_PREFIX="mpirun"
                ;;
  SLURM)  MPI_PREFIX="srun --propagate=STACK --mpi=pmi2"
                ;;
  *)  MPI_PREFIX="mpirun"
                ;;
esac

# Figure out default NP (1), or special settings if found in par_setup.dat
if [[ ${NP} == NONE ]]; then
  if test -f par_setup.dat ; then
    NP=$(head -n 1 par_setup.dat)
  else
    NP=0
  fi
fi

if [[ ${NP} == "" ]]; then NP="0"; fi


if [[ "${NP}" == "0" ]]; then
  NP=1
  MPI_PREFIX=""
fi

NPROC=$(echo ${NP} | cut -d':' -f1)
if [[ ${NPROC} == ${NP} ]]; then
  PPN=${NPROC}
  NODES=1
else
  PPN=$(echo ${NP} | cut -d':' -f2 |cut -d 'x' -f1)
  NODES=$(echo ${NP} | cut -d':' -f2 |cut -d 'x' -f2)
  if [[ ${NODES} == ${NP} ]]; then
    NODES=1
  fi
fi

NP=${NPROC}

if [[ "${NP}" ==  "${NODES}" ]]; then
  case ${SYSTEM} in
    PBS)   NODES=$(expr \( $NP - 1 \) / 16 + 1 )
           PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
           NP=$(expr $NODES \* $PPN )
           ;;
    MOAB)  NODES=$(expr \( $NP - 1 \) / 8 + 1 )
           PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
           NP=$(expr $NODES \* $PPN )
           ;;
    SLURM)  NODES=$(expr \( $NP - 1 \) / 24 + 1 )
           PPN=$(expr \( $NP - 1 \) / $NODES + 1 )
           #NP=$(expr $NODES \* $PPN )
           if [[ ${POSTPROCESS} -eq NONE ]]; then
             POSTPROCESS=slurm_postprocess.sh
           fi
           ;;
    *)     ;;
  esac
fi
NP=$(expr ${NODES} \* ${PPN})

#echo np=$NP nodes=$NODES ppn=$PPN

if [[ ${NPROC} -gt ${NP} ]]; then
  echo "Illegal to specify more processors than resources requested (${NPROC} > ${NODES}x${PPN})"
  exit 1
fi

echo "Building scripts for system ${SYSTEM} with MPI_PREFIX ${MPI_PREFIX} for ${NPROC} cpus on ${PPN}x${NODES} processors x nodes"

#case ${SYSTEM} in
#  SLURM) if [[ ${POSTPROCESS} -eq NONE ]]; then
#           POSTPROCESS=slurm_postprocess.sh
#         fi
#         ;;
#  *)     ;;
#esac

if [[ AUTOTITLE -eq 1 ]]; then
  TITLE=${SETUP}-${NODES}x${PPN}-$(basename ${ARG})
fi

RETITLE=1
if [[ ${TITLE} == 0 ]] ; then
  RETITLE=0
  TITLE=${SETUP}
fi

if [[ "x${TITLE}" == "x" ]] ; then
  TITLE=${SETUP}
fi


STDERR=${TITLE}.stderr
STDOUT=${TITLE}.stdout

if [[ "x${MPI_PREFIX}" != "x" ]] ; then
  case ${SYSTEM} in
    PBS)  MPI_PREFIX=${MPI_PREFIX}
        ;;
    SGE)  MPI_PREFIX=${MPI_PREFIX}
        ;;
    SLURM)  MPI_PREFIX=${MPI_PREFIX}
        ;;
    *) MPI_PREFIX="${MPI_PREFIX} -np ${NPROC}"
       ;;
  esac
fi

EMAIL=${MOSSCO_USER_EMAIL:-$(who am i |cut -f1 -d" ")@$(hostname)}

if [[ "x${WALLTIME}" == "x00:00:00" ]] ; then
  WALLTIME=$(predict_time $NP $SYSTEM $RESTART)
fi

if test -f ${NML}; then
  START=$(cat ${NML} | grep -v --regexp ' *!'| grep start | awk -F"'" '{print $2}' | awk -F" " '{print $1}')
  STOP=$(cat ${NML} | grep -v --regexp ' *!'| grep stop | awk -F"'" '{print $2}' | awk -F" " '{print $1}')

  echo $(timeInterval "$START" "$STOP")
fi

case ${SYSTEM} in
  PBS)
    QUEUE=mpi_64
    echo '#!/usr/bin/ksh' > pbs.sh
    cat << EOT >> pbs.sh
#PBS -N ${TITLE}
#PBS -S /usr/bin/ksh
#PBS -q ${QUEUE}
#PBS -l select=${NODES}:ncpus=${PPN}:mpiprocs=${PPN}
#PBS -l place=scatter:excl
#PBS -j oe
#PBS -r n
#PBS -e ${TITLE}.stderr
#PBS -o ${TITLE}.stderr
#PBS -W umask=000
#PBS -m e
#PBS -M ${EMAIL}

module load gcc/4.8.2
module load mpt/2.06

export SGI_MPI_HOME=${MPI_ROOT}
export PATH=${MOSSCO_SETUPDIR}/sns:${PATH}
export RAMFILES=1
export NPROC=${NPROC}

${MPI_PREFIX} ${EXE} ${NML}

cd \$PBS_O_WORKDIR
echo 'Working Directory     : '\$PBS_O_WORKDIR
echo 'Queue                 : '\$PBS_O_QUEUE
echo 'Job-ID                : '\$PBS_JOBID
echo 'Shell                 : '\$SHELL
echo 'MPI_ROOT              : '\$MPI_ROOT
echo 'License File          : '\$CDLMD_LICENSE_FILE
echo 'PBS-PATH              : '\$PBS_O_PATH
#
cat \$PBS_NODEFILE
EOT

;;
  SLURM)
    echo '#!/bin/bash -x' > slurm.sh
    cat << EOT >> slurm.sh
#SBATCH --ntasks=${NPROC}
#SBATCH --output=${TITLE}-%j.stdout
#SBATCH --error=${TITLE}-%j.stderr
#SBATCH --time=${WALLTIME}
#SBATCH --mail-user=${EMAIL}
#SBATCH --mail-type=ALL
#SBATCH --job-name=${TITLE}
EOT

    if [  $(echo $HOSTNAME |grep -c mlogin) == 1 ]; then

      # These are instructions for mistral.dkrz.de
      if [ ${QUEUE} == undefined ]; then QUEUE="compute2,compute"; fi

      echo \#SBATCH --cpus-per-task=2 >> slurm.sh
      echo \#SBATCH --account=$(groups | cut -d" " -f1) >> slurm.sh
      echo \#SBATCH --partition=${QUEUE}  >> slurm.sh

      echo export I_MPI_FABRICS=shm:dapl >> slurm.sh
      echo export I_MPI_FALLBACK=disable  >> slurm.sh
      echo export I_MPI_SLURM_EXT=1  >> slurm.sh
      echo export I_MPI_LARGE_SCALE_THRESHOLD=8192  >> slurm.sh
      echo export I_MPI_STATS=20   >> slurm.sh

    elif [  $(echo $HOSTNAME |grep -c juwels) == 1 ]; then
      # This is tested on juwels.fz-juelich.de
      if [ ${QUEUE} == undefined ]; then QUEUE=batch; fi
      echo \#SBATCH --account=$(groups | cut -d" " -f4) >> slurm.sh
      echo \#SBATCH --partition=${QUEUE} >> slurm.sh
      echo \#export OMP_NUM_THREADS=48 >> slurm.sh
      echo "" >> slurm.sh
      echo "# module load Intel/2019.0.117-GCC-7.3.0 ParaStationMPI/5.2.1-1" >> slurm.sh
      echo "# module load ESMF/7.1.0r" >> slurm.sh
    else
      # Tested on strand.hereon.de
      if [ ${QUEUE} == undefined ]; then QUEUE=pCluster; fi
      echo \#SBATCH --partition=${QUEUE} >> slurm.sh
      if [ ${NPROC} -gt 48 ]; then 
        echo \#SBATCH --exclusive >> slurm.sh
      fi
      echo \#export OMP_NUM_THREADS=48 >> slurm.sh
      echo "" >> slurm.sh
      echo "module load compilers/intel intelmpi" >> slurm.sh
    fi

    echo "" >> slurm.sh
    if [[ $RESTART -gt 1 ]]; then
      echo "module load nco  python/2.7-ve0" >> slurm.sh
      echo "" >> slurm.sh
      echo "# ncks prepare data files" >> slurm.sh
      echo "python prepare-hotstarts.py ${ARG} ${RESTART}"
      echo "# adjust mossco_run.nml " >> slurm.sh
      echo "# optionally copy-from restart directory" >> slurm.sh
      echo "# optionally copy-from restart directory" >> slurm.sh
      echo "# optionally copy-from restart directory" >> slurm.sh
    fi
    echo  ${MPI_PREFIX} ${EXE} ${NML}>> slurm.sh
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

${MPI_PREFIX} ${EXE} ${NML} > \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.stdout 2> \$PBS_O_WORKDIR/$TITLE.\$PBS_JOBID.stderr
EOT
;;
  SGE) cat << EOT > sge.sh
#!/bin/bash

#$ -N ${TITLE}
#$ -pe orte_rr $NPROC
#$ -m beas
#$ -M ${EMAIL}
#$ -cwd
#$ -V

cat \$PE_HOSTFILE

#mkdir -p ${OUTDIR}
#test -d ${OUTDIR} || (echo "Directory ${OUTDIR} could not be created" ; exit 1)

${MPI_PREFIX} ${EXE} ${NML} > ${STDOUT} 2> ${STDERR}
EOT
  ;;
esac

rm -rf PET[0-9].${TITLE} PET[0-9][0-9].${TITLE} PET[0-9][0-9][0-9].${TITLE} PET[0-9][0-9][0-9][0-9].${TITLE}
rm -f ${TITLE}*stdout ${TITLE}*stderr ${STDERR} ${STDOUT}

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

#if ! test -f ${NML} ; then cp mossco_run.nml ${NML}; fi

if test -f ${NML} ; then
  if [[ "${LOGLEVEL}" != "undefined" ]] ; then
    ${SED} -i 's/loglevel =.*/loglevel = "'${LOGLEVEL}'",/' ${NML}
    export loglevel="${LOGLEVEL}"
  fi
fi


if [[ ${RETITLE} != 0 ]] ; then

  if ! test -x ${SED}; then
    echo
    echo "ERROR: Cannot execute the sed program $SED"
    exit 1
  fi

  if test -f getm.inp ; then
    ${SED} -i "s/runid *=.*/runid = '${TITLE}',/" getm.inp
    if [[ "x${MPI_PREFIX}" != "x" ]] ; then
      ${SED} -i 's/parallel *=.*/parallel = .true.,/' getm.inp
      export parallel=True
    else
      ${SED} -i 's/parallel *=.*/parallel = .false.,/' getm.inp
      export parallel=False
    fi
  fi

  if test -f gotmrun.nml ; then
    ${SED} -i "s/title *=.*/title = '${TITLE}',/" gotmrun.nml
    ${SED} -i "s/out_fn *=.*/out_fn = '${TITLE}_gotm',/" gotmrun.nml
  fi

  if test -f ${NML} ; then
    ${SED} -i "s/title *=.*/title = '${TITLE}',/" ${NML}
  fi

  export runid="${TITLE}"
  export title="${TITLE}"
  export out_fn="${TITLE}_gotm"
  if [[ "x${MPI_PREFIX}" != "x" ]] ; then
    export parallel=True
  else
    export parallel=False
  fi

fi

if test -f ./par_setup.dat ; then
  if test -f ./Parallel/par_setup.${NPROC}p.dat ; then
    ln -sf ./Parallel/par_setup.${NPROC}p.dat par_setup.dat
    echo "Linked Parallel/par_setup.${NPROC}p.dat to par_setup.dat"
  else
    echo "Warning: check that par_setup.dat is correctly setup for ${NPROC} processors"
  fi
fi

for F in $(ls *.dim 2> /dev/null) ; do
  if test -f Parallel/${F%%.dim}.${NPROC}p.dim ; then
    ln -sf Parallel/${F%%.dim}.${NPROC}p.dim $F
    echo "Linked Parallel/${F%%.dim}.${NPROC}p.dim to $F"
  else
    echo "Warning: check that $F is correctly setup for ${NPROC} processors"
  fi
done

if ! test -f ${NML} ; then
  echo
  #echo "ERROR: Need file mossco_run.nml to run"
  #exit 1
  echo "Creating missing mossco_run.nml"
  make namelist_mossco
fi

if [[ ${BUILD_ONLY} == 1 ]] ; then
  exit 0
fi

case ${SYSTEM} in
  PBS)  if test $(which qsub 2> /dev/null)  ; then
           qsub pbs.sh
           echo "Job ${TITLE} submitted for system ${SYSTEM}"
         else cat pbs.sh ; fi
         ;;
  MOAB)  if test $(which msub 2> /dev/null)  ; then
           msub moab.sh
           echo "Job ${TITLE} submitted for system ${SYSTEM}"
         else cat moab.sh ; fi
         ;;
  SGE)   if test $(which qsub 2> /dev/null) ; then
           if [ ${QUEUE} == undefined ]; then QUEUE=$(select_sge_queue ${NP}); fi
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
           echo "Job ${TITLE} with jobid ${JOBID} submitted to queue ${QUEUE} for system ${SYSTEM}"
           if test -f ${POSTPROCESS}; then
             JOBID=$(sbatch --parsable --dependency=afterok:${JOBID} ${POSTPROCESS})
             echo "Postprocess job with jobid ${JOBID} submitted to queue ${QUEUE} for system ${SYSTEM}"
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
  BACKGROUND)  ${MPI_PREFIX} ${EXE} ${NML}  1>  ${STDOUT}  2> ${STDERR} &
         PID=$!
         echo "${MPI_PREFIX} ${EXE} ${NML} " '1>'  "${STDOUT}"  ' 2> ' "${STDERR}" ' &'
         echo "Job ${TITLE} with PID ${PID} interactively running in background"
         if [[ ${WAITTIME} -gt 0 ]]; then
           echo "Waiting for process ${PID} to finish"
           wait $PID
         fi
         ;;
  FOREGROUND)  ${MPI_PREFIX} ${EXE}  ${NML} 1>  ${STDOUT}  2> ${STDERR}
         echo "${MPI_PREFIX} ${EXE}  ${NML}" '1>'  "${STDOUT}"  ' 2> ' "${STDERR}"
         echo "Job ${TITLE} interactively running in foreground"
         ;;
  *)     echo "System ${SYSTEM} not defined in $0"; exit 1
         ;;
esac


test -f ${STDOUT} && tail -n 20 ${STDOUT}
test -f ${STDERR} && tail -n 20 ${STDERR}
test -f PET0.${TITLE} && tail -n 100 PET0.${TITLE}
