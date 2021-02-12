#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=1:mem=30gb

module load anaconda3/personal

cd /rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts

Rscript recoding_comorbid.R
