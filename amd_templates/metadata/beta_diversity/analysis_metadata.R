#!/usr/bin/env Rscript

localdir = getwd()

clustering_level = 'Genus'
analysis_type = 'beta_diversity'
knitr_options="
knitr::opts_chunk$set(
  echo=TRUE,
#  dpi=300,
  fig.width=6,
  fig.height=6
)"

relative_abundance_cutoff = 0.002
prevalence_cutoff = 0.1
min_count_cutoff = 0

#all_covariate_names = 'c("CaseString", "Gender", "AREDS", "Tissue_code", "AREDS")'
#age_lt_90_covariate_names = all_covariate_names


use_allbac = F

template_types = c(
  'beta_diversity',
  'beta_diversity_unifrac',
  'beta_diversity_unifrac_amd_only'
)


