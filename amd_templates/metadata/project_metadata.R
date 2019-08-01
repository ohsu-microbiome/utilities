### project_metadata.R

miseq_run_number = 278
miseq_project_prefix = paste0('miseq-', miseq_run_number)

home_dir = '~'
project_dir = file.path(home_dir, miseq_project_prefix)
dada2_table_dir = file.path(project_dir, "processed_data")
metadata_dir = file.path(project_dir, "metadata")
analysis_dir = file.path(project_dir, 'analysis')
tables_dir = file.path(analysis_dir, 'tables')

observational_variables_file = file.path(metadata_dir, 'observational_variables.R')

sample_data_file = file.path(
  metadata_dir,
  paste0(
    miseq_project_prefix,
    '_metadata_CNV_GA.tsv'
    )
  )
asv_table_file = file.path(
  dada2_table_dir,
  paste0(
    miseq_project_prefix,
    '_tall_asv_table.tsv'
    )
  )
taxonomy_table_file = file.path(
  dada2_table_dir,
  paste0(
    miseq_project_prefix,
    '_taxonomy_table.tsv'
    )
  )


### Alpha Diversity
#### ASV Clustered

##### Master Table
alpha_div_asv_clust_master_table = file.path(
  tables_dir,
  paste0(
    miseq_project_prefix,
    '_asv_clust_alpha_diversity_master_tables.xlsx'
    )
)

##### Two Group Stats
alpha_div_asv_clust_two_group_test_file = file.path(
  tables_dir,
  paste0(
    miseq_project_prefix,
    '_alpha_div_asv_clust_mann_whitney_unadjusted.xlsx'
    )
  )


##### Regression Stats

#### Genus Clustered

##### Master Table
alpha_div_asv_clust_master_table = file.path(
  tables_dir,
  paste0(
    miseq_project_prefix,
    '_genus_clust_alpha_diversity_master_tables.xlsx'
  )
)

##### Two Group Stats
alpha_div_genus_clust_two_group_test_file = file.path(
  tables_dir,
  paste0(
    miseq_project_prefix,
    '_alpha_div_genus_clust_mann_whitney_unadjusted.xlsx'
  )
)

##### Regression Stats





