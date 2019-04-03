library(dplyr)

# study_metadata_file = args[1]
# asv_table_file = args[2]
# taxonomy_table_file = args[3]

study_metadata_file = 'metadata/miseq-239_enhanced_map.tsv'
asv_table_file = 'processed_data/tables/miseq-239_asv_table.tsv'
taxonomy_table_file = 'processed_data/tables/miseq_239_taxonomy_table.tsv'

study_metadata = read.table(study_metadata_file, sep='\t')
study_metadata = 
  study_metadata %>%
  mutate(Age_le_90=ifelse(Age<=90, 'Y', 'N'))

asv_table = read.table(asv_table_file, sep='\t')
sampleIDs = study_metadata %>%
  select(SampleID) %>%
  unlist %>%
  as.character

rownames(asv_table) = sampleIDs
taxonomy_table = read.table(taxonomy_table_file, sep='\t')
  
t_select_metadata = 
  study_metadata %>%
  select(SampleID, Case, CaseString, Age_le_90, AREDS, Gender) %>%
  t %>%
  data.frame %>%
  tibble::rownames_to_column(var="tax_glom") %>%
  select(tax_glom, everything()) %>%
  data.frame

taxonomy_table = 
  taxonomy_table %>% 
  ### Create ASVs column from rownames
  tibble::rownames_to_column(var='ASVs')

tasv_table = 
  asv_table %>% 
  ### Transpose to put ASVs as rows
  t %>% 
  data.frame %>%
  ### Create ASVs column from rownames
  tibble::rownames_to_column(var='ASVs')

tax_abundance_table = 
  tasv_table %>% 
  inner_join(taxonomy_table, by='ASVs')

glommed_tax_abundacne_table = 
  tax_abundance_table %>% 
  ### Glom taxa ranks together
  mutate(tax_glom = paste(Phylum, Class, Order, Family, Genus, sep='|')) %>%
  ### Drop everything but the sampleIDs and tax_glom
  select(one_of(c('tax_glom', sampleIDs))) %>%
  ### Sum all the counts grouping by the tax_glom (effectively genus)
  group_by(tax_glom) %>% 
  summarize_all(sum)

lefse_table_all = rbind(as.matrix(t_select_metadata), as.matrix(glommed_tax_abundacne_table))

filename = 'analysis/make_tables/tables/miseq-239_lefse_all_table.tsv'
write.table(
  lefse_table_all, 
  file=filename,
  quote=F,
  row.names=F,
  col.names=F,
  sep='\t'
  )

age_le_90_mask = c(T, study_metadata[['Age']] <= 90)
lefse_table_le_90 = lefse_table_all[,age_le_90_mask]
filename = 'analysis/make_tables/tables/miseq-239_lefse_le_90_table.tsv'
write.table(
  lefse_table_le_90, 
  file=filename,
  quote=F,
  row.names=F,
  col.names=F,
  sep='\t'
  )

age_gt_90_mask = c(T, study_metadata[['Age']] > 90)
lefse_table_gt_90 = lefse_table_all[, age_gt_90_mask]
filename = 'analysis/make_tables/tables/miseq-239_lefse_gty_90_table.tsv'
write.table(
  lefse_table_gt_90, 
  file=filename,
  quote=F,
  row.names=F,
  col.names=F,
  sep='\t'
  )

areds_yes_mask = c(T, study_metadata[['AREDS']] == 'Y')
lefse_table_areds_yes = lefse_table_all[, areds_yes_mask]
filename = 'analysis/make_tables/tables/miseq-239_lefse_areds_yes_table.tsv'
write.table(
  lefse_table_areds_yes, 
  file=filename,
  quote=F,
  row.names=F,
  col.names=F,
  sep='\t'
  )

areds_no_mask = c(T, study_metadata[['AREDS']] == 'N')
lefse_table_areds_no = lefse_table_all[, areds_no_mask]
filename = 'analysis/make_tables/tables/miseq-239_lefse_areds_no_table.tsv'
write.table(
  lefse_table_areds_no, 
  file=filename,
  quote=F,
  row.names=F,
  col.names=F,
  sep='\t'
  )




