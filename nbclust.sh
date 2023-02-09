#!/bin/bash
#SBATCH -A b1042
#SBATCH -p genomics
#SBATCH --time=47:59:59
#SBATCH --mem=66G
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mail-user=yanyi.ding@northwestern.edu
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name="sepsis_nbclust"
#SBATCH --output=/home/ydn4687/sepsis/sepsis_nbclust.out

module purge
module load R

Rscript nbclust.R
