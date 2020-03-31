#!/usr/bin/env Rscript

localdir = getwd()

clustering_level = 'Phylum'
analysis_type = 'phyla'
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

regression_stats = c("pvalue", "effect_size", "std_err")
regression_transform = 'log'
log_regularizer = relative_abundance_cutoff

linear_regression_predictors = list(
  All=c('Age', 'Gender', 'Tissue_code', 'CFHrs10737680', 'CFHrs1061170', 'ARMS2rs10490924', 'SKIV2Lrs429608', 'AREDS'),
  Age_lt_90=c('Age', 'Gender', 'Tissue_code', 'CFHrs10737680', 'CFHrs1061170', 'ARMS2rs10490924', 'SKIV2Lrs429608', 'AREDS'),
  AMD_Only=c('Age', 'Gender', 'Tissue_code')
  )

logistic_regression_predictors = list(
  All=c('Age', 'Gender', 'Tissue_code'),
  Age_lt_90=c('Age', 'Gender', 'Tissue_code'),
  AMD_Only=c('Age', 'Gender', 'Tissue_code')
  )

use_allbac = F

template_types = c(
  'basic_data_prep',
  'raw_data_plots',
  'linear_regression',
  'linear_regression_plots',
  'logistic_regression',
  'two_group_tests',
  'two_group_plots'
#  'logistic_regression_plots'
)

