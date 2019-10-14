#!/usr/bin/env Rscript

localdir = getwd()

clustering_level = NULL
analysis_type = 'master_table'
knitr_options="
  knitr::opts_chunk$set(
  echo=TRUE,
  dpi=300,
  fig.width=12
)"

relative_abundance_cutoff = 0.002
prevalence_cutoff = 0.1
conda_env = 'base'
raw_exp_vars = NULL
calculated_exp_vars = NULL
use_allbac = F

template_types = c(
  'master_table_prep'
)

