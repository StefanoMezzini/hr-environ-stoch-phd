# Notes for running writing PBS scripts for UBC's Sockeye

## Sockeye parameters

* `-l` = resource parameters:
  * the max runtime is 7 days (7 * 24 = 168 hours), i.e. `168:00:00` (format is `hours:minutes:seconds`)
  * `select` is the number of compute nodes
  * `ncpus` is the total number of cores (use 32 or 40)
  * `mem` is the max amount of RAM (keep 1-3 GB lower than the max to allow the OS to run in the background)
* `-N` = job name
* `-A` = allocation (**do not change**)
* `-m` send a message following abort (`a`), begin (`b`), and end (`e`)
* `-M` email address to which messages should be sent
* `-o` file where console output should be written to if the script runs successfully
* `-e` file where error message should be written to if the script fails/aborts

* Directories are always read starting from the root, so start them from "`/home`" or "`/scratch`"
* `$USER` is the current user
* `$HOME` is the current home

## Loading software modules

* `source $HOME/miniconda3/etc/profile.d/conda.sh` loads `miniconda` from the current home
* `conda activate <module-name>` loads the software included in the module saved as `<module-name>` (replace text and angle brackets with the module name)

## Sourcing scripts

* `Rscript /scratch/st-mnoonan-1/file/location/script.R` runs the specified `R` script
* indicate the location of the script(s) starting from the root directory, i.e. start from `/scratch`
* save scripts in `/scratch`
* ensure everything needed by the script(s) is available locally; the compute nodes cannot access the internet!
