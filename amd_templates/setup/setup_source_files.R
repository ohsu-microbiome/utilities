#!/usr/bin/env Rscript

library(tidyverse)
library(magrittr)
library(stringr)

args = commandArgs(trailingOnly=TRUE)

analysis_dir = getwd()
print(sprintf("analysis_dir: %s", analysis_dir))
source(file.path(analysis_dir, 'analysis_metadata.R'))

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
utilities_dir = '~/utilities'

source(file.path(setup_script_dir, 'amd_project_utilities.R'))

project_root = findProjectRoot(getwd(), '.proj_root')
print(sprintf("project root: %s", project_root))
miseq_project_prefix = gsub("([\\/\\w\\-\\._]+\\/)","", project_root, perl=T)
print(sprintf("miseq project prefix: %s", miseq_project_prefix))
miseq_run_number = gsub(".*/miseq-([0-9]+)$", "\\1", project_root)
print(sprintf("miseq run number: %s", miseq_run_number))

project_metadata_file = file.path(project_root, "metadata", "project_metadata.R")
print(sprintf("project metadata file: %s", project_metadata_file))
print(file.exists(project_metadata_file))
source(project_metadata_file)

analysis_title = makeAnalysisTitle(analysis_type)
print(sprintf("analysis title: %s", analysis_title))

print('dada2_tables_dir')
print(dada2_tables_dir)
print('metadata_dir')
print(metadata_dir)

print("file templates")
print(asv_table_file_template)
print(taxonomy_table_file_template)
print(sample_data_file_template)

if (exists('phylogenetic_tree_file_template'))
{
  print(phylogenetic_tree_file_template)
}

print("real files")
asv_table_file = file.path(
  project_root,
  dada2_tables_dir,
  paste0(miseq_project_prefix, asv_table_file_template)
)
print(asv_table_file)
print(file.exists(asv_table_file))

taxonomy_table_file = file.path(
  project_root,
  dada2_tables_dir,
  paste0(miseq_project_prefix, taxonomy_table_file_template)
)
print(taxonomy_table_file)
print(file.exists(taxonomy_table_file))

sample_data_file = file.path(
  project_root,
  metadata_dir,
  paste0(miseq_project_prefix, sample_data_file_template)
)

if (exists('phylogenetic_tree_file_template'))
{
  phylogenetic_tree_file = file.path(
    project_root,
    dada2_tables_dir,
    paste0(miseq_project_prefix, phylogenetic_tree_file_template)
  )
}

print(sprintf('sample_data_file: %s', sample_data_file))
print(file.exists(sample_data_file))

### For some reason, analysis_dir is getting corrupted to "analysis" and I need to
### redefine it.
analysis_dir = getwd()

### Create run script
run_script = "#!/bin/bash\n\n"

print(sprintf('passing in analysis_dir %s', analysis_dir))
for (type in template_types)
{
  print(sprintf('template type: %s', type))

  print(sprintf('passing in analysis_dir %s', analysis_dir))

  new_source_file = generateSourceFile(
    template_type=type,
    analysis_type,
    analysis_dir=analysis_dir
    )

  run_script = paste0(run_script, "\n", "rmd ", new_source_file, "\n")

}

write(file='runall.sh', run_script)





