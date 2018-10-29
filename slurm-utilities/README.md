# slurm-utilities

## qsbatch
"Quick" `sbatch`. Or, a fancy version of `srun`. Submits a job
specified at the command line as an actual batch script. Includes
output end error log files.

**TO DO:**
- [ ] More customization for log files
- [ ] Does slurm have a meta log file for tracking run time and 
      other job-specific information?

## sinteract
Starts an interactive shell session on a compute node. It is a 
wrapper around `srun --pty bash` that allows you to easily specify
parameters for machine requirements and time. You can also
request a specific node in case you need to hop back into a 
particular compute node.


