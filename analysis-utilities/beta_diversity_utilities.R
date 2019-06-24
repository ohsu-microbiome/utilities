library(tidyverse)
library(magrittr)
library(ggplot2)


makeNMDSPlot = function(
  master_table,
  k=2,
  color,
  title_extra=''
)
{
  community_matrix = 
    master_table %>% 
    select(SampleName, filtered_taxa) %>% 
    column_to_rownames('SampleName') %>%
    as.matrix()
  
  nmds_data = metaMDS(community_matrix, k=2, engine='monoMDS', autotransform=T) 
  
  nmds_data$points %>%
    data.frame() %>%
    rownames_to_column('SampleName') %>% 
    inner_join(sample_metadata, by='SampleName') %>%
    ### plotting
    ggplot(aes_string(x="MDS1", y="MDS2", color=color)) +
    geom_point() + 
    ggtitle(paste('nMDS', title_extra))
}


makePCOAPlot = function(
  sample_taxa_table,
  method,
  color_group,
  taxa,
  title_extra=''
)
{
  # sample_taxa_table = master_table %>% filter(IGA %in% c('Neg', 'Pos'))
  # method = 'bray'
  # color_group = 'IGA'
  # taxa = filtered_taxa
  
  ### For title
  capitalized_method = paste0(toupper(substr(method, 1, 1)), substr(method, 2, nchar(method)))
  
  community_matrix = 
    sample_taxa_table %>% 
    column_to_rownames('SampleName') %>% 
    select(taxa)
  
  distances = 
    vegdist(community_matrix, diag=T, upper=T, method=method) %>%
    as.matrix() %>%
    as.data.frame()
  
  principal_components = 
    prcomp(distances) %>%
    summary() %>%
    .$x %>%
    data.frame() %>%
    select(PC1, PC2) %>%
    rownames_to_column('SampleName') %>%
    inner_join(sample_metadata, by='SampleName')
  
  principal_components %>%
    ggplot(aes_string(x="PC1", y="PC2", color=color_group)) + 
    geom_point() + 
    ggtitle(sprintf("PCoA %s Distance %s", capitalized_method, title_extra))
}

