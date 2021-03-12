
#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=8:mem=10gb
#PBS -N 1nodesevcores

module load anaconda3/personal
module load fix_unwritable_tmp
cd /rds/general/project/hda_students_data/live/Group3/TDS_Group3/Scripts/PLS
nchunks=8
Rscript CopyOfsPLS_calibration.R $nchunks

