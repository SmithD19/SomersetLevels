# SomersetLevels
This repository forms the basis of my analysis for the first chapter of my PhD at the University of Reading and UKCEH, under Dr. Beth Purse. 

## The Question
We would like to know how mosquitos respond to different environmental covariates (abiotic), and what impact biotic interactions have on the occurance probabilities of mosquito species.  

## The Data
Originally the data collected was to determine whether two different water management styles in the Somerset Levels impacted the abundance of mosquito species present, which may have had potential implications for nuisance biting and disease spread. 

67 sites were sampled across multiple seasons and multiple years from 2009 - 2011. At each site dip samples recorded the presence of any mosquito species and their potential predators. A range of environmental covariates were also recorded, ranging from plant species present, physical structure of the environment (shaded, cover, width, etc) climatic conditions (temp), and water chemistry (pH, turbidity, salinity, etc).

## Analysis
I attempt to use the HMSC workflow (https://www.helsinki.fi/en/researchgroups/statistical-ecology/hmsc), to produce joint models of species responses to all covariates and examine any residual coocurences between species. 

I do not use any trait or phylogentic data in this model. Aggregating trait data for mosquitoes would be useful int his context, but is a larger problem and remains to be explored in later chapters of my PhD.

## Prelim Results
We discovered that management level (or tiers) of the water bodies does not impact any species occurances and most variation was due to structural or water chemistry covariates. 

I have a functioning probit model for this data set, but the PSRF of Omega (residual correlation) remains too high to be trusted.

Abundance models show similar results, but chains fail to converge (PSRF) after 5 days on a supercomputing cluster (Does this just need more iterations?)

Hurdle modelling seems to converge fine but shows almost no interactions between covariates or species (This may be the case but seems unlikely). I think the hurdle model does,'t work to well because all spoecies seem fairly rare (15% of 300 sample sites are occupied) 

## This GitHub repo

Code 
 - This contains all the model code and data processing
 - Make sure to run the DataProcessing.R script first to generate the data if working from a scratch repo
 - Model scripts are named appropriately
 - Cross-Validation scripts beign with CV

Plots
 - Contain all the plots

Jobs 
 - Contains scripts to use with a SLURM cluster
 
 


