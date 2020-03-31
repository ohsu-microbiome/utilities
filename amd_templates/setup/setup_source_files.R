#!/usr/bin/env Rscript

library(tidyverse)
library(magrittr)
library(stringr)

args = commandArgs(trailingOnly=TRUE)

getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>%
    enframe(name = NULL) %>%
    separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    filter(key == "--file") %>%
    pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

print("getting location of setup_source_file.R (this file)")
setup_script_dir = getCurrentFileLocation()
print(sprintf("this script dir: %s", setup_script_dir))

source(file.path(setup_script_dir, 'amd_project_utilities.R'))

project_root = findProjectRoot(getwd(), '.proj_root')
print(sprintf("project root: %s", project_root))

print("****  sourcing project metadata  ****")
project_metadata_file = file.path(project_root, "metadata", "project_metadata.R")
print(sprintf("project metadata file: %s", project_metadata_file))
print(file.exists(project_metadata_file))
source(project_metadata_file)

# miseq_projeVct_prefix = gsub("([\\/\\w\\-\\._]+\\/)","", project_root, perl=T)
print(sprintf("miseq project prefix: %s", miseq_project_prefix))
# miseq_run_number = gsub("[^0-9]+([0-9]+).*", "\\1", miseq_project_prefix)
print(sprintf("miseq run number: %s", miseq_run_number))

print("****  getting analysis dir with getwd()  ****")
src_dir = getwd()
print(sprintf("src_dir: %s", src_dir))

print("****  sourcing analysis metadata  ****")
analysis_metadata_file = file.path(src_dir, "analysis_metadata.R")
print(sprintf("analysis metadata file: %s", analysis_metadata_file))
print(file.exists(analysis_metadata_file))
source(analysis_metadata_file)

print("****  Analysis Title and Subtitle  ****")
analysis_title = makeAnalysisTitle(analysis_type)
print(sprintf("analysis title: %s", analysis_title))
analysis_subtitle = makeAnalysisSubtitle(src_dir)
print(sprintf("analysis subtitle: %s", analysis_subtitle))

print('dada2_tables_dir')
print(dada2_tables_dir)
print('metadata_dir')
print(metadata_dir)

print("base data file names")
print(asv_table_file)
print(taxonomy_table_file)
print(sample_data_file)

if (exists('phylogenetic_tree_file_template'))
{
  print(phylogenetic_tree_file_template)
}

print("real files")
# asv_table_file = file.path(
#   project_root,
#   dada2_tables_dir,
#   paste0(miseq_project_prefix, asv_table_file_template)
# )
asv_table_file = file.path(
  project_root,
  dada2_tables_dir,
  asv_table_file
)
print(asv_table_file)
print(file.exists(asv_table_file))

# taxonomy_table_file = file.path(
#   project_root,
#   dada2_tables_dir,
#   paste0(miseq_project_prefix, taxonomy_table_file_template)
# )
taxonomy_table_file = file.path(
  project_root,
  dada2_tables_dir,
  taxonomy_table_file
)
print(taxonomy_table_file)
print(file.exists(taxonomy_table_file))

# sample_data_file = file.path(
#   project_root,
#   metadata_dir,
#   paste0(miseq_project_prefix, sample_data_file_template)
# )
sample_data_file = file.path(
  project_root,
  metadata_dir,
  sample_data_file
)
print(sample_data_file)
print(file.exists(sample_data_file))

# if (exists('phylogenetic_tree_file_template'))
# {
#   phylogenetic_tree_file = file.path(
#     project_root,
#     dada2_tables_dir,
#     paste0(miseq_project_prefix, phylogenetic_tree_file_template)
#   )
# }

if (exists('phylogenetic_tree_file'))
{
  phylogenetic_tree_file = file.path(
    project_root,
    dada2_tables_dir,
    phylogenetic_tree_file
  )
}



# ### For some reason, src_dir is getting corrupted to "analysis" and I need to
# ### redefine it.
# src_dir = getwd()
# print(sprintf("src_dir: %s", src_dir))
# # source(file.path(src_dir, 'analysis_metadata.R'))

### Create run script
run_script = "#!/bin/bash\n\n"

print(sprintf('passing in src_dir %s', src_dir))
print("****  template types  ****")
print(template_types)

for (type in template_types)
{
  print(sprintf('template type: %s', type))

  print(sprintf('passing in src_dir %s', src_dir))

  new_source_file = generateSourceFile(
    template_type=type,
    analysis_type,
    src_dir=src_dir
    )

  run_script = paste0(run_script, "\n", "rmd ", new_source_file, "\n")

}

write(file='runall.sh', run_script)





