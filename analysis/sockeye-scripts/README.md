# Notes for running writing PBS scripts for UBC's Sockeye

## Example PBS script

```
#!/bin/bash

# ------------Sockeye Parameters----------------- #

#PBS -l walltime=00:10:00,select=1:ncpus=40:mem=187gb
#PBS -N job-name
#PBS -A st-mnoonan-1
#PBS -m abe
#PBS -M me@ubc.ca
#PBS -o output.txt
#PBS -e error.txt

# ----------------Modules------------------------- #

source $HOME/miniconda3/etc/profile.d/conda.sh
conda activate stefano-ctmm

# -----------------My Commands-------------------- #

Rscript /scratch/my/directory/my-R-script.R
```

## Sockeye parameters

* `-l` = resource parameters:
  * `walltime` = the max run time. Max `walltime` is 7 days (7 * 24 = 168 hours), i.e. `168:00:00` (format is `hours:minutes:seconds`)
  * `select` is the number of compute nodes
  * `ncpus` is the total number of cores (use 32 or 40)
  * `mem` is the max amount of RAM (keep 1-3 GB lower than the max to allow the OS to run in the background)
* `-N` = job name
* `-A` = resource allocation (**use your (PI's) allocation**)
* `-m` = send a message when the job aborts (`a`), begins (`b`), and ends (`e`)
* `-M` = email address to which messages should be sent
* `-o` = file where console output should be written to if the script runs successfully
* `-e` = file where error messages and warnings should be written to (including if the script fails/aborts)

## Directory structure and commands

* Directories are always read starting from the root, so start them from "`/home`" or "`/scratch`"
* `$USER` is the current user (e.g., `qstat -u $USER` list the jobs running for `$USER`)
* `$HOME` is the current home (e.g., `cd $HOME` changes directory to the home directory)

## Loading software modules

* `source $HOME/miniconda3/etc/profile.d/conda.sh` loads `miniconda` from the current home
* `conda activate <module-name>` loads the software included in the module saved as `<module-name>` (replace text and angle brackets with the module name)

## Sourcing scripts

* `Rscript /scratch/st-mnoonan-1/file/location/script.R` runs the specified `R` script
* indicate the location of the script(s) starting from the root directory, e.g. start from `/scratch` or change the directory in the `R` script using `setwd('/scratch/my/dir')`
* save scripts in `/scratch`
* ensure everything needed by the script(s) is available locally; the compute nodes cannot access the internet!
