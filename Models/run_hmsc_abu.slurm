#!/bin/bash
#SBATCH --job-name=Run_hmsc_abu_med     # nom du job
##SBATCH -C v100-16g                 # decommenter pour reserver uniquement des GPU V100 16 Go
##SBATCH -C v100-32g                 # decommenter pour reserver uniquement des GPU V100 32 Go
##SBATCH --partition=gpu_p2          # decommenter pour la partition gpu_p2 (GPU V100 32 Go)
##SBATCH -C a100 # decommenter pour la partition gpu_p5 (GPU A100 80 Go)
#SBATCH --qos=qos_gpu-t4
#SBATCH --nodes=1                    # nombre de noeud
#SBATCH --ntasks-per-node=4          # nombre de tache MPI par noeud (= nombre de GPU par noeud)
#SBATCH --gres=gpu:4                 # nombre de GPU par nœud (max 8 avec gpu_p2, gpu_p5)B
#SBATCH --cpus-per-task=10           # nombre de CPU par tache
##SBATCH --cpus-per-task=3           # nombre de CPU par tache pour gpu_p2 (1/8 du noeud 8-GPU)
##SBATCH --cpus-per-task=8           # nombre de CPU par tache pour gpu_p5 (1/8 du noeud 8-GPU)
#SBATCH --hint=nomultithread         # hyperthreading desactive
#SBATCH --time=100:00:00              # temps d’execution maximum demande (HH:MM:SS)
#SBATCH --output=Run_hmsc_abu_med%j.out # nom du fichier de sortie
#SBATCH --error=Run_hmsc_abu_med%j.out  # nom du fichier d'erreur (ici commun avec la sortie)
#SBATCH -A gqd@v100
##SBATCH --account=103909@cpu
#SBATCH --array=0-3

#set -x
# Nettoyage des modules charges en interactif et herites par defaut
module purge
 
# Decommenter la commande module suivante si vous utilisez la partition "gpu_p5"
# pour avoir acces aux modules compatibles avec cette partition
#module load cpuarch/amd

cp -r ~/HMSC_med/run_hmsc_abu.slurm $SCRATCH
cp -r /lustre/fswork/projects/rech/gqd/uwm38ts/HMSC/models/unfitted_models $SCRATCH
cp -r /lustre/fswork/projects/rech/gqd/uwm38ts/HMSC/models/init_file_abu_thin_4000_samples_250_chains_4.rds $SCRATCH

cd $SCRATCH

# Chargement des modules
module load tensorflow-gpu/py3/2.11.0
which python

SAM=250
THIN=4000
TRANS=500000

input_path="init_file_abu_thin_4000_samples_250_chains_4.rds"
output_path="post_file_abu_thin_4000_samples_250_chains_4.rds"
output_path=$(printf "abu_thin_4000_samples_250_chains_4_post_chain%.2d_file.rds" $SLURM_ARRAY_TASK_ID)

srun python -u -m hmsc.run_gibbs_sampler --input $input_path --output $output_path --samples $SAM --transient $TRANS --thin $THIN --verbose 100 --chain $SLURM_ARRAY_TASK_ID >& compute_abu_$SLURM_ARRAY_TASK_mpa_ID.log

 
# Echo des commandes lancees
set -x

