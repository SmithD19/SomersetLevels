#!/bin/bash

#SBATCH --output=R-%x.%j.out
#SBATCH --error=R-%x.%j.err

#these are all the default values anyway 
#SBATCH --ntasks=1 
#SBATCH --cpus-per-task=4  
#SBATCH --threads-per-core=1

# time limit and memory allocation 
#SBATCH --time=30-00:00:00 
#SBATCH --mem=12G 

#SBATCH --mail-type=ALL
#SBATCH --mail-user=d.smith34178@gmail.com

# Execute
Rscript Scripts/CV-Abundance.R
