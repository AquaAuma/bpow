#!/bin/bash
#SBATCH --job-name=associate_missing_regions_test1.0
#SBATCH --output="bpow/slurm-%j.out"
#SBATCH --mail-type=ALL # send me emails when starting and ending
#SBATCH --time 10:00:00
#SBATCH --mem=150g # 5GB of RAM
#SBATCH --nodes=1 --ntasks=1 --cpus-per-task=8
#SBATCH -p general,pi_jetz,bigmem

echo '-------------------------------'
cd ${SLURM_SUBMIT_DIR}
echo ${SLURM_SUBMIT_DIR}
echo Running on host $(hostname)
echo Time is $(date)
echo SLURM_NODES are $(echo ${SLURM_NODELIST})
echo '-------------------------------'
echo -e '\n\n'
​
module load R/4.0.3-foss-2020b
module load GDAL/3.2.1-foss-2020b

Rscript /gpfs/ysm/home/aam238/bpow/e.bis_add_missing_regions_hpc.R