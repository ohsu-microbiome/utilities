library(tidyverse)
library(magrittr)
library(stringr)

####
metadata_folder_found = F
search_dir = getwd()

while (!metadata_folder_found & current_dir != "/")
{
  search_dir = gsub("\\/[a-zA-Z0-9\\-_\\.]+$", "", search_dir)

  print(sprintf("search directory: %s", search_dir))

  sub_dirs = list.dirs(path=search_dir, recursive = F)
  print(sub_dirs)
  metadata_dir = grep("metadata", sub_dirs, value=T)
  metadata_folder_found = length(metadata_dir) != 0
}

source(file.path(search_dir, "metadata/project_metadata.R"))
####

cluster_level = 'asv'
analysis_type = 'alpha_diversity'
src_dir = file.path(analysis_dir, analysis_type, cluster_level, 'src')

utilities_dir = '~/utilities'
templates_dir = file.path(utilities_dir, 'amd_templates/analysis', analysis_type)
template_file = file.path(templates_dir, 'alpha_diversity/alpha_diversity_data_prep_template.Rmd')

source_dir = file.path(analysis_dir, cluster_level, 'src')

#asv_table_file
#taxonomy_table_file
#sample_data_file

relative_abundance_cutoff = 0.02
prevalence_cutoff = 0
conda_env = 'alphadiv'
knitr_options = ""
libraries = c(
  'tidyverse',
  'magrittr',
  'ggplot2',
  'ggbeeswarm',
  'DT',
  'stringr'
)

knitr_options="
opts_chunk$set(
  echo=TRUE,
  dpi=300,
  fig.width=12
)"

templates = list.files(path=templates_dir, pattern="*.Rmd")
for (template in templates)
{
  template_file = file.path(templates_dir, template)
  print(template_file)
  new_source_file = file.path(src_dir, paste0(miseq_project_prefix, "_", template))
  print(new_source_file)
}

template_text =
  readLines(template_file) %>%
  paste(collapse="\n") %>%
  as.character() %>%
  str_interp()

# template_text = "${cluster_level}" %>% str_interp()


writeLines(
  template_text,
  new_source_file
)



