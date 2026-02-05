#!/bin/bash

# ------------------------------------------------------------------------------
# SLURM Job Configuration - Presence/Absence Model
# ------------------------------------------------------------------------------

#SBATCH --job-name=Run_hmsc_mpa_med
#SBATCH --qos=qos_gpu-t4
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --gres=gpu:4
#SBATCH --cpus-per-task=10
#SBATCH --hint=nomultithread
#SBATCH --time=100:00:00
#SBATCH --output=Run_hmsc_mpa_med%j.out
#SBATCH --error=Run_hmsc_mpa_med%j.out
#SBATCH -A gqd@v100
#SBATCH --array=0-3

# ------------------------------------------------------------------------------
# Module Setup
# ------------------------------------------------------------------------------

# Clean default modules
module purge

# Load required modules
module load tensorflow-gpu/py3/2.11.0
which python

# ------------------------------------------------------------------------------
# Copy Files to Scratch
# ------------------------------------------------------------------------------

cp -r ~/HMSC_med/run_hmsc_mpa.slurm $SCRATCH
cp -r /lustre/fswork/projects/rech/gqd/uwm38ts/HMSC/models/unfitted_models $SCRATCH
cp -r /lustre/fswork/projects/rech/gqd/uwm38ts/HMSC/models/init_file_mpa_thin_4000_samples_250_chains_4.rds $SCRATCH

cd $SCRATCH

# ------------------------------------------------------------------------------
# MCMC Parameters
# ------------------------------------------------------------------------------

SAM=250
THIN=4000
TRANS=500000

input_path="init_file_mpa_thin_4000_samples_250_chains_4.rds"
output_path=$(printf "mpa_thin_4000_samples_250_chains_4_post_chain%.2d_file.rds" $SLURM_ARRAY_TASK_ID)

# ------------------------------------------------------------------------------
# Run Gibbs Sampler
# ------------------------------------------------------------------------------

srun python -u -m hmsc.run_gibbs_sampler --input $input_path --output $output_path --samples $SAM --transient $TRANS --thin $THIN --verbose 100 --chain $SLURM_ARRAY_TASK_ID >& compute_mpa_$SLURM_ARRAY_TASK_ID.log

set -x

