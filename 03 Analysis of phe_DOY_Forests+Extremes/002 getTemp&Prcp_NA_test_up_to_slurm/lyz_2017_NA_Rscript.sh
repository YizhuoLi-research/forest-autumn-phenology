#!/bin/bash
#SBATCH -J 2017_NA_Rscript  #workingname --Rscript
#SBATCH -p hebhcnormal02
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -o log.%j
#SBATCH -e log.%j

source ~/software/R-4.3.2/install/env.sh

Rscript /public/home/ac6u713xut/LiYZ/Graduation_Thesis/2017-getTemp_Prcp_NA_test.r



