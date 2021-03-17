
#PBS -l walltime=72:00:00
#PBS -l select=1:ncpus=2:mem=40gb


module load anaconda3/personal
cd /rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS
Rscript sPLS_calibration.R

