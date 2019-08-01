library(tidyverse)
library(magrittr)
library(vegan)
library(ggplot2)
library(ggbeeswarm)

calculateAlphaDiversity = function(
    taxa_abundance,
    indices=c('shannon')
  )
{

  print("in calculateAlphaDiversity")
  # sample_names_df =
  #   sample_names %>%
  #   setNames(sample_names) %>%
  #   data.frame() %>%
  #   setNames('SampleName') %>%
  #   remove_rownames()

  sapply(indices, function(index)
  {
    taxa_abundance %>%
      select(sample_names) %>%
      t() %>%
      diversity(index=index)
  }) %>%
  data.frame() %>%
  rownames_to_column('SampleName')

}


calculateAlphaDiversity2 = function(
  sample_metadata_taxa_table,
  taxa_columns,
  indices=c('shannon')
)
{

  # sample_metadata_taxa_table = master_table
  # taxa_columns = filtered_taxa
  # indices = c('shannon', 'observed')

  print("in calculateAlphaDiversity")
  # sample_names_df =
  #   sample_names %>%
  #   setNames(sample_names) %>%
  #   data.frame() %>%
  #   setNames('SampleName') %>%
  #   remove_rownames()

  sapply(indices, function(index)
  {
    temp =
      sample_metadata_taxa_table %>%
      column_to_rownames('SampleName') %>%
      select(taxa_columns)

    if (index=='observed')
    {
      return(rowSums(temp != 0))
    }else
    {
      return(diversity(temp, index=index))
    }
  }) %>%
    data.frame() %>%
    rownames_to_column('SampleName')
}

geom_pval_annotation = function(annotations)
{
  geom_text(
    data=annotation_df,
    aes(x=xloc, y=yloc, label=pvals)
    )
}

# indices=c('Firmicutes', 'Bacteroidetes')
# title_template="Firmicutes and Bacteroidetes: AMD Only"
# annotation_data=pval_annotations %>% filter(TestGroup=='AMD_Only')
# variables=amd_only_variables
# # variables = variables['CFHrs10737680']
# master_table = amd_only_master_table
#
# plotTwoGroupTests(
#   variables=variables,
#   master_table=master_table,
#   indices=indices,
#   title_template=title_template,
#   annotation_data=annotation_data
# )


