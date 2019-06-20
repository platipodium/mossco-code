#!/bin/bash -x

#SBATCH --job-name=schism_mossco    # Specify job name
#SBATCH --partition=pAll    # Specify partition name
#SBATCH --ntasks=4
##SBATCH --ntasks-per-node=1
#SBATCH --time=00:30:00        # Set a limit on the total run time
#SBATCH --wait-all-nodes=1     # start job, when all nodes are available
#SBATCH --mail-type=FAIL       # Notify user by email in case of job failure
#SBATCH --mail-user=carsten.lemmen@hzg.de  # Set your e−mail address
#SBATCH --output=log.stderr    # File name for standard output
#SBATCH --error=log.stdout     # File name for standard error output


srun -l --propagate=STACK ${MOSSCO_DIR}/examples/esmf/schism/schism_mossco

# wait until all nodes/file-actions are settled
wait

