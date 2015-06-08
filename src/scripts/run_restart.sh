#! /bin/bash
#
# filename run_restart.sh


# Specify prefix of restart file name (without .nc suffix)
PREFIX=restart_gffrpn

# Specify number of processors
NP=1

# Specify title of simulation
TITLE=dl-m-gffrpn

# SPECIFY kind of coupling
COUPLING=gffrpn

# SPECIFY month interval
MM=1

# ---------------------------------------
#
SED=${SED:-$(which gsed)} 2> /dev/null
SED=${SED:-$(which sed)} 2> /dev/null


STARTDATE=$(grep 'start =' mossco_run.nml |cut -d'"' -f2)
STOPDATE=$(grep 'stop =' mossco_run.nml |cut -d'"' -f2)

START=${STARTDATE}
Y=${START%%-*}
REM=${START#*-}
M=${REM%%-*}
REM=${REM#*-}

MX=$(expr ${M} + ${MM} - 1)
YX=$(expr $Y + $MX   / 12)
MX=$(expr $MX % 12 + 1)

if (( ${MX} < 10 )) ; then
  MX=0${MX}
fi

STOP=${YX}-${MX}-${REM}
RE="s/stop =.*/stop = \"${STOP}\",/"
RE="'"$RE"'"

echo $RE

${SED} -i "${RE}" mossco_run.nml

# Test whether a restart file is present.  If not, then assume we're starting mossco for the first
# time

if ! test -f ${PREFIX}.nc ; then
	rm -f restart.cfg
	echo "Running $0 for the first time from $START to $STOP"
else
	echo "Restarting from $START to $STOP"
fi

#mossco -n ${NP} -t ${TITLE} ${COUPLING}

INPUT=${PREFIX}.nc ## todo: add index for multi-proc output

# Extract the last timestep
#${MOSSCO_DIR}/src/scripts/cut_revert_last_time.sh ${INPUT}

# Find out what file was produced and link it to a generic name, then create
# the file restart.cfg pointing to this file
OUT=$(ls -1rt ${PREFIX}_*.nc |tail -1)

ln -sf ${OUT} restart.nc

cat << EOT > restart.cfg
filename: restart.nc
EOT

# Now manipulate mossco_run.nml
START=$(ncdump -v date_string ${INPUT} | tail -2 | head -1 | cut -d'"' -f2)

${SED} -i 's/start =.*/start = "'"${START}"'",/' mossco_run.nml

# Call myself

exec $0
