library(dplyr)
library(DESeq2)
library(ggplot2)
library(tidyr)

getAggregatedTaxaRankCounts = function(
    taxonomy_table, # Table with ASVs and taxa ranks as columns, taxa names as values
    asv_table, # Table with ASVs and sample IDs as columns, counts as values
    lowest_rank="Genus" # Lowest taxa rank to group by and aggregate on
  )
{
  
  ### Result: table with columns for taxa names, agglommerated taxa names, and counts 
  ### for each sample.
  
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  print(lowest_rank)
  lowest_rank_index = match(lowest_rank, all_ranks)
  ranks_to_glom = all_ranks[1:lowest_rank_index]
  print("ranks_to_glom")
  print(ranks_to_glom)
  
  print("creating tax abundance table")
  tax_abundance_table = 
    ### Join asv_table and taxonomy_table on ASVs to get sample abundances and rank names
    asv_table %>% 
    inner_join(taxonomy_table, by='ASVs') %>% 
    ### Drop everything but the sampleIDs and requested taxa
    select(one_of(sampleIDs, ranks_to_glom))  %>%
    ### Sum all the counts grouping by the glommed_taxa (effectively genus)
    group_by(.dots=ranks_to_glom) %>%
    summarize_all(sum) %>%
    ### Glom taxa ranks together for additinoal column
    mutate(glommed_taxa = paste(!!!syms(ranks_to_glom), sep='_')) %>%
    ### Fix hyphens in taxa names so they don't mess up column names later
    mutate(glommed_taxa = gsub('-', '_dash_', glommed_taxa)) %>%
    ### Fix slashes in taxa names so they don't mess up column names later
    mutate(glommed_taxa = gsub('/', '_slash_', glommed_taxa)) %>%
    ### Select again so glommed_taxa is first row (better way?)
    ### Could glom earlier and then group by ranks and glommed_taxa
    select(one_of('glommed_taxa', sampleIDs, ranks_to_glom))  %>%
    ### Convert to dataframe (instead of tibble)
    data.frame()
  
  # print(dim(tax_abundance_table))
  
  return(tax_abundance_table)
}


getMasterTable = function(
    taxononmy_table, 
    asv_table, metadata, 
    rank
  )
{
  
  ### Table combining agglomerated taxa counts and metadata. For each
  ### sample, columns include metadata followed by agglomerated taxa names.
  ### Values are counts.
  
  sampleIDs = as.vector(metadata[['SampleID']])
  
  aggregated_counts = aggregatedTaxaRankCounts(taxonomy_table, asv_table, rank)
  
  master_table = 
    aggregated_counts %>%
    ### Fix hyphens in taxa names so they don't mess up column names later
    mutate(glommed_taxa = gsub('-', '_dash_', glommed_taxa)) %>%
    ### Fix slashes in taxa names so they don't mess up column names later
    mutate(glommed_taxa = gsub('/', '_slash_', glommed_taxa)) %>%    
    select(one_of('glommed_taxa', sampleIDs)) %>%
    ### Add rownames which will become colnames (headers) when transposed
    tibble::column_to_rownames('glommed_taxa') %>% 
    ### Transpose and coerce back to dataframe
    t() %>% 
    data.frame() %>% 
    tibble::remove_rownames() %>% 
    mutate(SampleID=factor(sampleIDs)) %>%
    inner_join(metadata, by='SampleID') %>% 
    select(one_of(colnames(metadata), aggregated_counts[['glommed_taxa']]))
  
  return(master_table)
}

runDeseq = function(
    aggregated_counts, # table with columns for taxa names, agglommerated taxa names, and counts for each sample.
    metadata, # sample metadata
    design,  # design formulat or matrix
    return_obj='df' # Whether to return dataframe (default 'df') or 'raw' deseq results object
)
{
  ### Run deseq on aggregated taxa rank counts. Input has counts for glommed taxa names (aggregated
  ### over those taxa) for each sample.
   
  sampleIDs = as.vector(metadata[['SampleID']])
  
  counts =
    aggregated_counts %>%
    ### Remove zeros so log does not cause errors
    replace(.==0, 1) %>%
    ### Change tax glom to rownames
    tibble::remove_rownames() %>%
    tibble::column_to_rownames('glommed_taxa') %>%
    select(one_of(sampleIDs)) %>%
    data.frame()
  
  # cat('dim counts:', dim(counts))
  # cat('dim metadata:', dim(metadata))
  
  dds <- DESeqDataSetFromMatrix(
    countData=counts,
    colData=metadata,
    design= design
  )
  
  deseq_obj = DESeq(
    dds,
    test='Wald',
    fitType='mean'
  )
  
  deseq_results = results(deseq_obj)
  deseq_results_column_descriptions = deseq_results@elementMetadata$description
  
  deseq_results_df = 
    data.frame(deseq_results@listData) %>%
    mutate('glommed_taxa' = deseq_results@rownames) %>%
    inner_join(aggregated_counts, by='glommed_taxa') %>%
    select(-one_of(sampleIDs))

  print(dim(deseq_results_df))
  print(colnames(deseq_results_df))
  
  if (return_obj=='raw')
  {
    return(deseq_results)
  } else if(return_obj=='df')
  {
    return(deseq_results_df)
  } else
  {
    message('ERROR: Unknown return type')
    return(FALSE)
  }
  
}


getSignificantTaxaCounts = function(
    deseq_results_df,
    aggregated_counts,
    cutoff_expr
)
{
  
  ### Simple wrapper to filter deseq results based on an expression. Expression can 
  ### be anything, but the intention is that it will filter pvalue or qvalue. Joining
  ### with the aggregated counts returns the counts for only the significant taxa.
  # print(head(deseq_results_df))
  # print(cutoff_expr)
  
  significant = 
    deseq_results_df %>% 
    filter_(cutoff_expr) %>% 
    select(glommed_taxa)

  # print(head(significant))
  
  glommed_taxa = significant[['glommed_taxa']]
  # print("glommed_taxa")
  # print(glommed_taxa)
  
  significant_counts_gathered_taxa = 
    inner_join(significant, aggregated_counts,  by=c("glommed_taxa")) %>%
    select(one_of(sampleIDs), glommed_taxa) %>%
    tibble::column_to_rownames('glommed_taxa') %>% 
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column('SampleID') %>%
    inner_join(metadata, by='SampleID')
  
  return(significant_counts_gathered_taxa)
}

plotTaxaCounts = function(counts, glommed_taxa, formula)
{
  ### Creates a faceted set of bar plots of the taxa abundance in each sample.
  ### Accepts a formula to use for faceting. Right now, the LHS has to be Taxa,
  ### and only one feature on the RHS.
  
  # print('head counts')
  # print(head(counts))

  # print("splitting formula")
  split_formula = as.character(formula)
  lhs = split_formula[2] %>% trimws()
  rhs = split_formula[3]
  rhs_pieces = strsplit(rhs, '+', fixed=T)[[1]] %>% trimws()
  # cat('lhs:', lhs, 'rhs pices:', rhs_pieces, '\n')
  
  kept_columns = c('SampleID', rhs_pieces)
  # print("kept columns")
  # print(kept_columns)
  
  # gathered_data =  
  #   counts %>%
  #   select(glommed_taxa, one_of(kept_columns)) %>%
    # gather(key='Taxa', value='Counts', -kept_columns, factor_key=T) 
  
  gathered_data =  
    counts %>%
    select(one_of(glommed_taxa, kept_columns)) %>%
    gather(key='Taxa', value='Counts', -kept_columns, factor_key=T) 
  
  # gathered_data = 
  #   counts %>%
  #   select(SampleID, one_of(glommed_taxa), CaseString) %>%
  #   gather(key='Taxa', value='Counts', -kept_columns) %>%
  #   arrange(SampleID, Counts)
  
  # print(colnames(gathered_data))
  # print(head(gathered_data))
  
  plot_obj = ggplot(gathered_data, aes(x=SampleID, y=Counts)) + 
    geom_bar(stat='identity') + 
    facet_grid(formula, scale='free_y') +
    theme(strip.text.y=element_text(angle=0, hjust=1))
  
  return(plot_obj)
}

plotDeseqLogFoldChangeBarplot = function(
  deseq_results_df,
  pvalue_cutoff=1.0,
  padj_cutoff=1.0,
  fill='pvalue',
  topN=20
)
{
  ### Makes a horizontal bar chart of log2 fold-changes returned from deseq analysis.
  ### Accepts filtering criteria for pvalue, qvalue (adjusted p value), and how many
  ### of the most significant taxa to plot. Additionally accepts a fill variable for
  ### coloring the bars.
  ###
  ### Hypothetically, you could add columns to the input df for additional coloring
  ### options. This has not been tested.
  
  # print(pvalue_cutoff)
  # print(padj_cutoff)
  top_N_logfold = 
    deseq_results_df %>%
    ### Apply p-value cutoffs
    filter(pvalue<=pvalue_cutoff, padj<=padj_cutoff) %>%
    mutate(glommed_taxa = factor(glommed_taxa, levels=glommed_taxa)) %>%
    arrange(abs(log2FoldChange)) %>%
    top_n(n=-topN) %>%
    arrange(log2FoldChange) %>%
    mutate(glommed_taxa = factor(glommed_taxa, levels=glommed_taxa))
  
  if (dim(top_N_logfold)[1] < topN)
  {
    topN = dim(top_N_logfold)[1]
    message = sprintf(
      "Only %i taxa meet the conditions: p<=%0.2f and q<=%0.2f",
      topN,
      pvalue_cutoff,
      padj_cutoff
    )
  }
  
  title = sprintf(
    "%i largest Absolute Log2 Fold-Change for p<=%0.2f and q<=%0.2f",
    topN,
    pvalue_cutoff,
    padj_cutoff
    )

  # ggplot(data=top_N_logfold, aes(x=glommed_taxa, y=log2FoldChange, fill=padj)) +
  ggplot(data=top_N_logfold, aes_string(x='glommed_taxa', y='log2FoldChange', fill=fill)) +
    geom_bar(stat='identity') +
    coord_flip() + 
    ggtitle(title)
}





