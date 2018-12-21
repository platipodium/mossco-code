#!/bin/bash

#$ -N schism
#$ -pe orte_rr 1
#$ -cwd
#$ -V

cat $PE_HOSTFILE

mpirun ./schism_mossco > schism_mossco.stdout 2> schism_mossco.stderr
