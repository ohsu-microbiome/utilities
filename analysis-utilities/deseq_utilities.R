library(dplyr)
library(DESeq2)
library(ggplot2)
library(tidyr)

getAggregatedTaxaRankCounts = function(
  taxonomy_table, # Table with ASVs and taxa ranks as columns, taxa names as values
  asv_table, # Table with ASVs and sample IDs as columns, counts as values
  lowest_rank="Genus", # Lowest taxa rank to group by and aggregate on
  clean=T # remove taxa where lowest rank value is NA
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
    ### Sum all the counts grouping by the axa (effectively genus)
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
  
  if (clean)
  {
    tax_abundance_table = 
      tax_abundance_table %>%
      filter_(sprintf("!is.na(%s)", lowest_rank))
  }
  
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
  lowest_rank='Phylum',
  design,  # design formulat or matrix
  return_obj='df' # Whether to return dataframe (default 'df') or 'raw' deseq results object
)
{
  ### Run deseq on aggregated taxa rank counts. Input has counts for glommed taxa names (aggregated
  ### over those taxa) for each sample.
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  
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
    ### Collect a structure with the deseq_results
    data.frame(deseq_results@listData) %>%
    ### Collect the glommed_taxa from the deseq_results
    mutate('glommed_taxa' = deseq_results@rownames) %>%
    ### Join to aggregated counts for further plotting
    inner_join(aggregated_counts, by='glommed_taxa') %>%
    ### Drop the sampleIDs which were included in the aggregated counts
    ### Samples are irrelevant now--just want the taxa counts and stats
    select(-one_of(sampleIDs))
  
  ### Make short glommed names with only Phylum and lowest rank.
  ### Use these for plots because full glommed_taxa don't fit
  if (match(lowest_rank, all_ranks) == 1)
  {
    ### Lowest rank taxa is Phylum, so no ranks to glom
    ### But still need this variable as it will be referred to
    ### in plotting
    deseq_results_df =
      deseq_results_df %>%
      mutate(short_glommed_taxa = Phylum)
  } else
  {
    ### Glom Phylum with lowest rank to make a short name
    ### There are cases where these are not unique, so use
    ### `make.unique` to uniqify them
    deseq_results_df =
      deseq_results_df %>%
      mutate_(short_glommed_taxa = sprintf('paste0(Phylum, "_", %s)',lowest_rank)) %>%
      mutate(short_glommed_taxa = make.unique(short_glommed_taxa))
  }


  # print(dim(deseq_results_df))
  # print(colnames(deseq_results_df))
  
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

plotTaxaCounts = function(
  counts, 
  significant_taxa, 
  taxa_rank,
  formula,
  normalize=F
)
{
  ### Creates a faceted set of bar plots of the taxa abundance in each sample.
  ### Accepts a formula to use for faceting. Right now, the LHS has to be Taxa,
  ### and only one feature on the RHS.
  
  # print('head counts')
  # print(head(counts))
  
  if (normalize)
  {
    counts = counts %>% mutate_at(vars(significant_taxa), funs(./sum(.)))
  }
  
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
    select(one_of(significant_taxa, kept_columns)) %>%
    gather(key='Taxa', value='Counts', -kept_columns, factor_key=T)
  
  gathered_data$Taxa = sapply(gathered_data$Taxa, shortenTaxaName)
  
  plot_title = 
    sprintf('%s level relative abundance for p<=0.05 significant taxa', taxa_rank)
  plot_obj = ggplot(gathered_data, aes(x=SampleID, y=Counts)) + 
    geom_bar(stat='identity') + 
    facet_grid(formula, scale='free') +
    theme(strip.text.y=element_text(angle=0, hjust=1, size=8)) +
    ggtitle(plot_title)
  
  return(plot_obj)
}

plotDeseqLogFoldChangeBarplot = function(
  deseq_results_df,
  lowest_rank='Phylum',
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
    filter(
      pvalue<=pvalue_cutoff, 
      padj<=padj_cutoff
    ) %>%
    ### Sort by biggest fold change magnitudes + OR -
    arrange(abs(log2FoldChange)) %>%
    ### Scrape the top N
    top_n(n=-topN) %>%
    ### Now order by actual fold change + to -
    arrange(log2FoldChange) %>%
    ### Make glommed taxa a factor and set levels to current order
    ### Otherwise, the bars will plot in alphabetical order
    mutate(short_glommed_taxa = factor(short_glommed_taxa, levels=short_glommed_taxa))
  
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
  
  most_minus = top_N_logfold$log2FoldChange[1]
  print(most_minus)
  most_plus = top_N_logfold$log2FoldChange[topN]
  print(most_plus)
  print(topN)
  
  ### Create dynamic title
  title = sprintf(
    "%s Level Log2(AMD/Control) Abundance (p<=%0.2f and q<=%0.2f)",
    lowest_rank,
    pvalue_cutoff,
    padj_cutoff
  )

  # ggplot(data=top_N_logfold, aes(x=glommed_taxa, y=log2FoldChange, fill=padj)) +
  plot_obj = ggplot(data=top_N_logfold, aes_string(x='short_glommed_taxa', y='log2FoldChange', fill=fill)) +
    geom_bar(stat='identity') +
    coord_flip() +
    ggtitle(title) +
    ylab("Log2 AMD/Control") +
    xlab("Significant Taxa") + 
    theme(
      axis.text.x = element_text(size=7),
      axis.text.y = element_text(size=7),
      plot.title = element_text(hjust=0, size=8)
    ) + 
    annotate("text", x=topN, y=1.1*most_minus/2, label = "Reduced in AMD", size=3, fontface=2) + 
    annotate("text", x=1, y=1.1*most_plus/2, label = "Increased in AMD", size=3, fontface=2)
  
  print(sprintf('annotate("text", x=%i, y=%0.2f, label = "Reduced in AMD", size=6)', topN, most_minus/2))
  
  return(plot_obj)
}

printFancyKableTable = function(
  data,
  caption
)
{
  kable(
    data, 
    caption=caption
  )  %>%
    kable_styling(font_size=9) %>%
    scroll_box(
      width = "100%", 
      height = "300px"
    )
}


shortenTaxaName = function(name)
{
  # print("shortenTaxaName")
  name = as.character(name)
  split_name = strsplit(name, "_") %>% unlist
  if (length(split_name)==1)
  {
    short_name = split_name
  } else
  {
    first = split_name[1]
    last = tail(split_name, n=1)
    short_name = paste0(first,'_',last)
  }
  return(short_name)
}


shortenTaxaNames = function(name, use_names=T)
{
  # print("shortenTaxaName")
  result = sapply(name, function(temp_name)
  {
    temp_name = as.character(temp_name)
    split_name = strsplit(temp_name, "_") %>% unlist
    if (length(split_name)==1)
    {
      short_name = split_name
    } else
    {
      first = split_name[1]
      last = tail(split_name, n=1)
      short_name = paste0(first,'_',last)
    }
    return(short_name) 
  },
  USE.NAMES=use_names
  )
  
  return(result)
}

running_var = function(df, variable)
{
  df = 
    df %>% 
    mutate(
      N=row_number(), 
      running_var=cumsum(A^2)/(N-1) -2*cumsum(A)*cummean(A)/(N-1) + N*cummean(A)^2/(N-1)
      ) %>%
    select(-N)
  
  return(df)
}