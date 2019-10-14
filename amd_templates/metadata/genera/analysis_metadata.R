#!/usr/bin/env Rscript

localdir = getwd()

clustering_level = 'Genus'
analysis_type = 'genus'
knitr_options="
  knitr::opts_chunk$set(
  echo=TRUE,
  dpi=300,
  fig.width=12
)"

relative_abundance_cutoff = 0.002
prevalence_cutoff = 0.1
min_count_cutoff = 0

raw_exp_vars="filtered_taxa"
calculated_exp_vars = c()

regression_transform = 'log'
log_regularizer = relative_abundance_cutoff

use_allbac = F

template_types = c(
  'basic_data_prep',
  'raw_data_plots',
  'linear_regression',
  'linear_regression_plots',
  'logistic_regression'
#  'two_group_tests',
#  'two_group_plots'
#  'logistic_regression_plots'
)

