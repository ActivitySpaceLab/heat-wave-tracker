#!/bin/sh

# making sure gdal path is correct
export PATH=/home/soft/gdal-2.3.2/bin/:$PATH
export LD_LIBRARY_PATH=/home/soft/gdal-2.3.2/lib/:$LD_LIBRARY_PATH

# starting in project directory
cd ~/research/heat-wave-tracker

# pull in any pending commits
git pull origin main

/home/soft/R-4.1.0/bin/R CMD BATCH --no-save --no-restore code/hourly_monitor.R logs/hourly_monitor.out 

/home/soft/R-4.1.0/bin/R CMD BATCH --no-save --no-restore render_hourly_monitor.R logs/render_hourly_monitor.out 


# Commit and push the log files from this latest run
git add --all
git commit -m 'new data and log files (cluster - automated)'
git pull origin main
git push origin main

# run using:
# qsub -q ceab -pe make 1 -l h_vmem=8G -m bea -M johnrbpalmer@gmail.com ~/research/heat-wave-tracker/monitor_heat.sh
