
#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=2:mem=15gb


module load anaconda3/personal
cd /rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS
Rscript gPLS_stability_selection.1.R

