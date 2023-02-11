#!/bin/bash
#SBATCH -A b1042
#SBATCH -p genomics
#SBATCH --time=47:59:59
#SBATCH --mem=100G
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mail-user=yanyi.ding@northwestern.edu     ## change email
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name="sepsis_nbclust_ind"
#SBATCH --output=/home/ydn4687/sepsis/sepsis_nbclust_ind.out   ## change directory

module purge
module load R

Rscript nbclust_ind.R  ## change file name
