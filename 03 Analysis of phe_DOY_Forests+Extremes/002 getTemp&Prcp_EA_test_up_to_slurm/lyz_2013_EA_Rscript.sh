#!/bin/bash
#SBATCH -J 2013_EA_Rscript  #workingname --Rscript
#SBATCH -p hebhcnormal02
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -o log.%j
#SBATCH -e log.%j
#SBATCH --exclusive

source ~/software/R-4.3.2/install/env.sh

Rscript /public/home/ac6u713xut/LiYZ/Graduation_Thesis/2013-getTemp_Prcp_EA_test.r



