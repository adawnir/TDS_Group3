#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=2:mem=20gb
#PBS -J 1-12

module load anaconda3/personal
source activate R-env
cd /rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/strat_townsend

m=$PBS_ARRAY_INDEX # model index

Rscript strat_townsend_spls.R $m
