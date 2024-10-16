#!/bin/bash
#SBATCH --job-name=run_801_to_1000
#SBATCH --output="bpow/slurm-%j.out"
#SBATCH --mail-type=ALL # send me emails when starting and ending
#SBATCH --time 24:00:00
#SBATCH --mem=150G # 150GB of RAM
#SBATCH --nodes=1 --ntasks=1 --cpus-per-task=10
#SBATCH -p general,pi_jetz,bigmem
#SBATCH --requeue

echo '-------------------------------'
cd ${SLURM_SUBMIT_DIR}
echo ${SLURM_SUBMIT_DIR}
echo Running on host $(hostname)
echo Time is $(date)
echo SLURM_NODES are $(echo ${SLURM_NODELIST})
echo '-------------------------------'
echo -e '\n\n'

module load R/4.0.3-foss-2020b
module load GDAL/3.2.1-foss-2020b

Rscript /gpfs/ysm/home/aam238/bpow/e.add_missing_regions_hpc.R