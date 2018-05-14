#!/bin/bash -x

#SBATCH --job-name=schism_mossco    # Specify job name
#SBATCH --partition=shared    # Specify partition name
#SBATCH --ntasks=4
##SBATCH --ntasks-per-node=1
#SBATCH --time=00:30:00        # Set a limit on the total run time
#SBATCH --wait-all-nodes=1     # start job, when all nodes are available
#SBATCH --mail-type=FAIL       # Notify user by email in case of job failure
#SBATCH --mail-user=carsten.lemmen@hzg.de  # Set your e−mail address
#SBATCH --account=gg0877       # Charge resources on this project account
#SBATCH --output=log.stderr    # File name for standard output
#SBATCH --error=log.stdout     # File name for standard error output

## use Intel MPI
module load intel/17.0.1
module load intelmpi/5.1.3.223
module load python/2.7-ve0

export I_MPI_PMI_LIBRARY=/use/lib64/libmpi.so

srun -l --propagate=STACK --cpu_bind=verbose,cores --distribution=block:cyclic ${MOSSCO_DIR}/examples/esmf/schism/schism_mossco

# wait until all nodes/file-actions are settled
wait

