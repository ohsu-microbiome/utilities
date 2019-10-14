#!/usr/bin/env Rscript

localdir = getwd()

clustering_level = 'Genus'
analysis_type = 'prevotella'
knitr_options="
  knitr::opts_chunk$set(
  echo=TRUE,
  dpi=300,
  fig.width=12
)"

relative_abundance_cutoff = 0.002
prevalence_cutoff = 0.1
min_count_cutoff = 0

raw_exp_vars='c()'
calculated_exp_vars = 'c("FractionPrevotellaAll", "FractionBacteroidetes", "FractionPrevotellaBacteroidetes")'
test_categorical_variables = 'c("PctPrevotella_gt_1", "PctPrevotella_gt_5", "PctPrevotella_gt_10")'
reference_categorical_variables = 'c("CaseString", "Gender", "AREDS", "Tissue_code", "ARMS2rs10490924", "CFHrs1061170", "CFHrs10737680", "SKIV2Lrs429608")'

regression_transform = 'log'
log_regularizer = relative_abundance_cutoff

use_allbac = F

template_types = c(
  'prevotella_data_prep',
  'linear_regression',
  'linear_regression_plots',
  'logistic_regression',
  'two_group_tests',
  'two_group_plots',
  'chisquare_tests'
)




