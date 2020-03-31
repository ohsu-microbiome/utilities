miseq_run_number = 278
miseq_project_prefix = paste0('miseq-', miseq_run_number)

home_dir = '~'
project_dir = file.path(home_dir, miseq_project_prefix)
metadata_dir = file.path(project_dir, "metadata")

source(file.path(metadata_dir, 'project_metadata.R'))
