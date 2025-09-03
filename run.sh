#!/bin/bash -l
#SBATCH -J neko_mixed_4_vreman
#SBATCH -t 02:00:00
#SBATCH --ntasks-per-node=8
#SBATCH --nodes 1
#SBATCH -p shared
#SBATCH -A naiss2024-1-3
#SBATCH --mail-type=BEGIN,END,FAIL

if [ ! -d logfiles ]; then
    mkdir logfiles
fi

d="$(date +%F_%H-%M-%S)"
srun -u -n 8 ./neko mixed.case > logfiles/logfile.log${d}
