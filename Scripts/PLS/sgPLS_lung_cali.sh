
#PBS -l walltime=60:00:00
#PBS -l select=1:ncpus=1:mem=10gb


module load anaconda3/personal
cd /rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS
Rscript sgPLS_lung_cali.R

