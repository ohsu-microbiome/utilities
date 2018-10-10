# utilities
Various scripts for microbiome work: slurm tools, conda build scripts, exacloud tools, etc.

For ease of use, add the following to your `.bashrc`

tmb="../teamMicrobiome/"
utils=$tmb/utilities
export PATH="$utils:$utils/conda_tools:$utils/project-utilities:$utils/exacloud-utilities:$utils/slurm-utilities:$PATH"
