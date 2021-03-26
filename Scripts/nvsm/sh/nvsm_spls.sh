#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=2:mem=20gb
#PBS -J 1-4

# i ran 4 here and that makes no sense - but I also ran the denoised ones I guess
module load anaconda3/personal
source activate R-env
cd /rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/nvsm

m=$PBS_ARRAY_INDEX # model index

Rscript nvsm_spls.R $m
