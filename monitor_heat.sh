#!/bin/sh

# making sure gdal path is correct
# FOR CEAB CLUSTER ONLY - TEMP OFF
# export PATH=/home/soft/gdal-2.3.2/bin/:$PATH
# export LD_LIBRARY_PATH=/home/soft/gdal-2.3.2/lib/:$LD_LIBRARY_PATH

# starting in project directory
cd /home/palmer/research/heat-wave-tracker

# pull in any pending commits
git pull origin main

source /home/palmer/.profile

# FOR CEAB CLUSTER ONLY - TEMP OFF
# /home/soft/R-4.1.0/bin/R CMD BATCH --no-save --no-restore code/heat_waves_selected_cities.R logs/heat_waves_selected_cities.out 
R CMD BATCH --no-save --no-restore code/heat_waves_selected_cities.R logs/heat_waves_selected_cities.out 


# FOR CEAB CLUSTER ONLY - TEMP OFF
# /home/soft/R-4.1.0/bin/R CMD BATCH --no-save --no-restore render_index.R logs/render_index.out 
R CMD BATCH --no-save --no-restore render_index.R logs/render_index.out 

# Commit and push the log files from this latest run
git add --all
git commit -m 'new data and log files (cluster - automated)'
git pull origin main
git push origin main

# run using:
# qsub -q ceab -pe make 1 -l h_vmem=8G -m bea -M johnrbpalmer@gmail.com ~/research/heat-wave-tracker/monitor_heat.sh
