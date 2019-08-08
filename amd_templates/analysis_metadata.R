#!/usr/bin/env Rscript

localdir = getwd()

clustering_level = 'asv'
analysis_type = 'igaseq'
knitr_options="
opts_chunk$set(
echo=TRUE,
dpi=300,
fig.width=12
)"

relative_abundance_cutoff = 0.02
prevalence_cutoff = 0
conda_env = 'alphadiv'
raw_exp_vars = c('IgA_Pos', 'IgA_neg', 'AllBac')
calculated_exp_vars = c('IgA_Index', 'ICI_Score', 'Alt_Index')

stats = c("pvalues", "effect_sizes", "std_errors")
use_parametric = "F"
adjustment_method = "BH"
response_var_name = "Taxa"

template_types = c(
  'igaseq_data_prep',
  'taxa_regression'
)

