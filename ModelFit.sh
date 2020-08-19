#!/bin/bash

# the job name and output file
#SBATCH --job-name=SeasonalAbun #(default is the script name)

#these are all the default values anyway 
#SBATCH --ntasks=1 
#SBATCH --cpus-per-task=4  
#SBATCH --threads-per-core=1 

# time limit and memory allocation 
#SBATCH --time=10-00:00:00 
#SBATCH --mem=4G 

#SBATCH --mail-type=ALL
#SBATCH --mail-user=d.smith34178@gmail.com

# Execute
Rscript Scripts/ModelFit.R
