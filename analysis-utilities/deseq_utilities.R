library(DESeq2)
library(ggplot2)
library(tidyverse)
library(magrittr)
rename = dplyr::rename



runDeseq = function(
  aggregated_counts, # table with columns for taxa names, agglommerated taxa names, and counts for each sample.
  metadata, # sample metadata
  lowest_rank='Phylum',
  design,  # design formula or matrix
  return_obj='df' # Whether to return dataframe (default 'df') or 'raw' deseq results object
)
{
  # print(design)
  # print(as.character(design))
  # print(paste(as.character(design), collapse=""))

  im_here = sprintf("in runDesqe with: \n
                    \taggregated_counts=%s \n
                    \tmetadata=%s \n
                    \tlowest_rank=%s \n
                    \tdesign=%s \n
                    \treturn_obj=%s \n",
                    paste(dim(aggregated_counts), collapse=" "),
                    paste(dim(metadata), collapse=" "),
                    lowest_rank,
                    paste(as.character(design), collapse=""),
                    return_obj
  )
  writeLines(im_here)

  ### Run deseq on aggregated taxa rank counts. Input has counts for glommed taxa names (aggregated
  ### over those taxa) for each sample.
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  print("design")
  print(design)

  sampleIDs = as.vector(metadata[['SampleID']])

  taxa_counts =
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

  dds = DESeqDataSetFromMatrix(
    countData=taxa_counts,
    colData=metadata,
    design=design
  )

  deseq_obj = DESeq(
    dds,
    test='Wald',
    fitType='mean'
  )

  print(resultsNames(deseq_obj))
  deseq_results = results(deseq_obj, tidy=T)
  deseq_results_column_descriptions = deseq_results@elementMetadata$description

  deseq_results_df =
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

  if (return_obj=='all')
  {
    deseq_data = list(
      deseq_results_df=deseq_results_df,
      deseq_obj=deseq_obj,
      taxa_counts=taxa_counts
    )
    return(deseq_data)
  } else if(return_obj=='df')
  {
    return(deseq_results_df)
  } else
  {
    message('ERROR: Unknown return type')
    return(FALSE)
  }

}


getDeseqObj = function(
  asv_table,
  taxonomy_table,
  metadata,
  lowest_rank='Genus',
  design
)
{

  ### Run deseq on aggregated taxa rank counts. Input has counts for glommed taxa names (aggregated
  ### over those taxa) for each sample.
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  print("design")
  print(design)

  sampleIDs = metadata %>% pull(SampleID) %>% as.vector()

  print("getting aggregated counts")
  aggregated_counts = getAggregatedTaxaRankCounts(
    asv_table=asv_table,
    taxonomy_table=taxonomy_table,
    metadata=metadata,
    lowest_rank=lowest_rank,
    clean=T
  )

  print("getting taxa counts")
  taxa_counts =
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

  dds = DESeqDataSetFromMatrix(
    countData=taxa_counts,
    colData=metadata,
    design=design
  )

  deseq_obj = DESeq(
    dds,
    test='Wald',
    fitType='mean'
  )

  # results_df =
  #   results(deseq_obj, contrast=contrast) %>%
  #   data.frame() %>%
  #   ### Collect the glommed_taxa from the deseq_results
  #   mutate('glommed_taxa' = rownames(deseq_obj)) %>%
  #   ### Join to aggregated counts for further plotting
  #   inner_join(aggregated_counts, by='glommed_taxa') %>%
  #   ### Drop the sampleIDs which were included in the aggregated counts
  #   ### Samples are irrelevant now--just want the taxa counts and stats
  #   select(-one_of(sampleIDs), -short_glommed_taxa)
  #
  # if (return=='all')
  # {
  #   deseq_data = list(
  #     results_df=results_df,
  #     deseq_obj=deseq_obj,
  #     taxa_counts=taxa_counts
  #   )
  #   return(deseq_data)
  # } else if(return=='obj')
  # {
  #   return(deseq_obj)
  # } else
  # {
  #   message('ERROR: Unknown return type')
  #   return(FALSE)
  # }

  return(deseq_obj)

}


runDeseq2 = function(
  aggregated_counts, # table with columns for taxa names, agglommerated taxa names, and counts for each sample.
  metadata, # sample metadata
  lowest_rank='Phylum',
  design,  # design formulat or matrix
  contrast,
  return_obj='df' # Whether to return dataframe (default 'df') or 'all' deseq results object
)
{
  # print(design)
  # print(as.character(design))
  # print(paste(as.character(design), collapse=""))

  im_here = sprintf("in getMultivariateDeseqResults with: \n
                    \taggregated_counts=%s \n
                    \tmetadata=%s \n
                    \tlowest_rank=%s \n
                    \tdesign=%s \n
                    \treturn_obj=%s \n",
                    paste(dim(aggregated_counts), collapse=" "),
                    paste(dim(metadata), collapse=" "),
                    lowest_rank,
                    paste(as.character(design), collapse=""),
                    return_obj
  )
  writeLines(im_here)

  ### Run deseq on aggregated taxa rank counts. Input has counts for glommed taxa names (aggregated
  ### over those taxa) for each sample.
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  print("design")
  print(design)

  sampleIDs = as.vector(metadata[['SampleID']])

  print("getting taxa counts")
  taxa_counts =
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

  print("creating deseq dataset")
  dds = DESeqDataSetFromMatrix(
    countData=taxa_counts,
    colData=metadata,
    design=design
  )

  print("running deseq")
  deseq_obj = DESeq(
    dds,
    test='Wald',
    fitType='mean'
  )

  print(resultsNames(deseq_obj))
  deseq_results = results(deseq_obj, contrast=contrast)
  deseq_results_column_descriptions = deseq_results@elementMetadata$description

  print("creating results df")
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

  if (return_obj=='all')
  {
    deseq_data = list(
      deseq_results_df=deseq_results_df,
      deseq_obj=deseq_obj,
      taxa_counts=taxa_counts
    )
    return(deseq_data)
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

  significant_glommed_taxa =
    deseq_results_df %>%
    filter(cutoff_expr) %>%
    pull(glommed_taxa)

  # print(head(significant))

  # glommed_taxa = significant[['glommed_taxa']]
  # print("glommed_taxa")
  # print(glommed_taxa)

  significant_counts_gathered_taxa =
    inner_join(significant_glommed_taxa, aggregated_counts,  by=c("glommed_taxa")) %>%
    select(one_of(sampleIDs), glommed_taxa) %>%
    tibble::column_to_rownames('glommed_taxa') %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column('SampleID') %>%
    inner_join(metadata, by='SampleID')

  return(significant_counts_gathered_taxa)
}



plotDeseqLogFoldChangeBarplot = function(
  deseq_results_df,
  lowest_rank='Phylum',
  variable_name,
  variable_levels,
  pvalue_cutoff,
  padj_cutoff,
  fill_col='pvalue',
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

  print(pvalue_cutoff)
  print(padj_cutoff)

  if (variable_name != "")
  {
    pvalue_cutoff_expr = paste0('pvalue', '.', variable_name, '<', pvalue_cutoff)
    padj_cutoff_expr = paste0('padj', '.', variable_name, '<', padj_cutoff)
    abs_l2fc = paste0('abs(log2FoldChange', '.', variable_name, ')')
    l2fc = paste0('log2FoldChange', '.', variable_name)
    fill_string = paste0(fill_col, '.', variable_name)
  } else
  {
    pvalue_cutoff_expr = paste0('pvalue', '<', pvalue_cutoff)
    padj_cutoff_expr = paste0('padj', '<', padj_cutoff)
    abs_l2fc = 'abs(log2FoldChange)'
    l2fc = 'log2FoldChange'
    fill_string = fill_col
  }

  print("data strings")
  print(pvalue_cutoff_expr)
  print(padj_cutoff_expr)
  print(abs_l2fc)
  print(l2fc)
  print(fill_string)

  print("getting top_N_logfold")
  top_N_logfold =
    deseq_results_df %>%
    ### Apply p-value cutoffs
    filter(
      .data(pvalue_cutoff_expr),
      .data(padj_cutoff_expr)
    ) %>%
    ### Sort by biggest fold change magnitudes + OR -
    arrange_(abs_l2fc) %>%
    ### Scrape the top N
    top_n(n=-topN) %>%
    ### Now order by actual fold change + to -
    arrange_(l2fc) %>%
    ### Make glommed taxa a factor and set levels to current order
    ### Otherwise, the bars will plot in alphabetical order
    mutate(short_glommed_taxa = factor(short_glommed_taxa, levels=short_glommed_taxa))

  print(dim(top_N_logfold))
  print(colnames(top_N_logfold))

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

  print('Creating title')
  ### Create dynamic title
  title = sprintf(
    "%s Level Log2(%s/%s) Abundance (p<=%0.2f and q<=%0.2f)",
    lowest_rank,
    variable_levels[2],
    variable_levels[1],
    pvalue_cutoff,
    padj_cutoff
  )

  print("plotting")
  # ggplot(data=top_N_logfold, aes(x=glommed_taxa, y=log2FoldChange, fill=padj)) +
  plot_obj = ggplot(
    data=top_N_logfold,
    aes_string(
      x='short_glommed_taxa',
      y=l2fc,
      fill=fill_string
    )) +
    geom_bar(stat='identity') +
    coord_flip() +
    ggtitle(title) +
    ylab("Log2 AMD/Control") +
    xlab("Significant Taxa") +
    theme(
      axis.text.x = element_text(size=7),
      axis.text.y = element_text(size=7),
      plot.title = element_text(hjust=0, size=10, margin=margin(unit = "in"))
    ) +
    annotate("text",
             x=topN,
             y=1.1*most_minus/2,
             label = paste0("Reduced in ", variable_levels[2]),
             size=3,
             fontface=2
             ) +
    annotate("text",
             x=1,
             y=1.1*most_plus/2,
             label = paste0("Increased in ", variable_levels[2]),
             size=3,
             fontface=2
             )

    # https://stackoverflow.com/a/22000100/188963

  print(sprintf('annotate("text", x=%i, y=%0.2f, label = "Reduced in AMD", size=6)', topN, most_minus/2))

  return(plot_obj)
}


plotDeseqLogFoldChangeBarplot2 = function(
    results_df,
    lowest_rank='Phylum',
    contrast,
    pvalue_cutoff,
    padj_cutoff,
    fill_col='pvalue',
    max_num_taxa=NA
  )
{
  ### Makes a horizontal bar chart of log2 fold-changes returned from deseq analysis.
  ### Accepts filtering criteria for pvalue, qvalue (adjusted p value), and how many
  ### of the most significant taxa to plot. Additionally accepts a fill variable for
  ### coloring the bars.
  ###
  ### Hypothetically, you could add columns to the input df for additional coloring
  ### options. This has not been tested.

  print("getting top_N_logfold")
  top_N_logfold =
    results_df %>%
    ### Apply p-value cutoffs
    filter(
      pvalue<pvalue_cutoff,
      padj<padj_cutoff
    ) %>%
    ### Sort by biggest fold change magnitudes + OR -
    arrange(abs(log2FoldChange)) %>%
    ### Now order by actual fold change + to -
    arrange(log2FoldChange) %>%
    ### Make glommed taxa a factor and set levels to current order
    ### Otherwise, the bars will plot in alphabetical order
    mutate(short_glommed_taxa = factor(short_glommed_taxa, levels=short_glommed_taxa))

  print(dim(top_N_logfold))
  print(colnames(top_N_logfold))

  # if (dim(top_N_logfold)[1] < topN)
  # {
  #   topN = dim(top_N_logfold)[1]
  #   message = sprintf(
  #     "Only %i taxa meet the conditions: p<=%0.2f and q<=%0.2f",
  #     topN,
  #     pvalue_cutoff,
  #     padj_cutoff
  #   )
  # }

  num_taxa = length(top_N_logfold$log2FoldChange)

  most_minus = top_N_logfold$log2FoldChange[1]
  print(most_minus)
  most_plus = top_N_logfold$log2FoldChange[num_taxa]
  print(most_plus)

  print('Creating title')
  ### Create dynamic title
  title = sprintf(
    "%s Level Log2(%s/%s) Abundance (p<=%0.2f and q<=%0.2f)",
    lowest_rank,
    contrast[2],
    contrast[3],
    pvalue_cutoff,
    padj_cutoff
    )

  print("plotting")
  # ggplot(data=top_N_logfold, aes(x=glommed_taxa, y=log2FoldChange, fill=padj)) +
  plot_obj = ggplot(
    data=top_N_logfold,
    aes_string(
      x='short_glommed_taxa',
      y='log2FoldChange',
      fill=fill_col
    )) +
    geom_bar(stat='identity') +
    coord_flip() +
    ggtitle(title) +
    ylab("Log2 AMD/Control") +
    xlab("Significant Taxa") +
    theme(
      axis.text.x = element_text(size=7),
      axis.text.y = element_text(size=7),
      plot.title = element_text(hjust=0, size=8, margin=margin(unit = "in"))
    ) +
    annotate("text",
             x=num_taxa,
             y=1.1*most_minus/2,
             label = paste0("Reduced in ", contrast[2]),
             size=3,
             fontface=2
    ) +
    annotate("text",
             x=1,
             y=1.1*most_plus/2,
             label = paste0("Increased in ", contrast[2]),
             size=3,
             fontface=2
    )

  # https://stackoverflow.com/a/22000100/188963

  # print(sprintf('annotate("text", x=%i, y=%0.2f, label = "Reduced in AMD", size=6)', topN, most_minus/2))

  return(plot_obj)
}




collectDeseqData = function(
  lowest_rank='Phylum',
  pvalue_cutoff=1,
  padj_cutoff=1,
  design
)
{

  im_here = sprintf("in collectDeseqData with: \n
                    \tlowest_rank=%s \n
                    \tpvalue_cutoff=%s \n
                    \tpadj_cutoff=%s\n
                    \tdesign=%s",
                    lowest_rank, pvalue_cutoff, padj_cutoff,
                    paste(as.character(design), collapse="")
  )
  writeLines(im_here)

  # lowest_rank = 'Phylum'
  # pvalue_cutoff=1
  # padj_cutoff=1

  print(lowest_rank)
  print(pvalue_cutoff)
  print(padj_cutoff)

  ### Collect counts aggregated at a specific taxonomic rank
  aggregated_counts = getAggregatedTaxaRankCounts(
    taxonomy_table=taxonomy_table,
    asv_table=asv_table,
    lowest_rank=lowest_rank,
    clean=T
  )

  ## Run deseq and collect the results. Order by p-value
  deseq_data = runDeseq(
    aggregated_counts=aggregated_counts,
    lowest_rank=lowest_rank,
    metadata=metadata,
    design=design,
    return_obj='all'
  )

  deseq_obj = deseq_data$deseq_obj
  deseq_results_df = deseq_data$deseq_results_df %>% arrange(pvalue)
  taxa_counts = deseq_data$taxa_counts

  # print(sprintf("pvalue<%0.2f", pvalue_cutoff))

  ### Filter the counts for significance and return dataframe with taxa
  ### as columns (see documentation in getSignificanTaxaCounts function)
  significant_taxa_counts = getSignificantTaxaCounts(
    deseq_results_df,
    aggregated_counts,
    sprintf("pvalue<%0.2f", pvalue_cutoff)
  )

  ### Collect list of significant taxa from deseq results
  significant_taxa =
    deseq_results_df %>%
    filter(pvalue<=pvalue_cutoff) %>%
    select(glommed_taxa) %>%
    pull(glommed_taxa)
  print("dim sig taxa")
  print(length(significant_taxa))

  return(list(
    aggregated_counts=aggregated_counts,
    deseq_obj=deseq_obj,
    deseq_results_df=deseq_results_df,
    taxa_counts=taxa_counts,
    significant_taxa_counts=significant_taxa_counts,
    significant_taxa=significant_taxa,
    sampleIDs=sampleIDs
  ))
}

writeDeseqData = function(basename, deseq_results_df, force=T)
{
  print(dim(deseq_results_df))
  print(colnames(deseq_results_df))


  filename = paste0('tables/', basename, '.tsv')
  print(filename)
  if (!file.exists(filename) | force==TRUE)
  {
    write.table(
      deseq_results_df,
      file=filename,
      quote=F,
      col.names=T,
      row.names=F,
      sep='\t'
    )
  }

  filename = paste0('tables/', basename, '.xlsx')
  print(filename)
  if(!file.exists(filename) | force==TRUE)
  {
    write.xlsx(
      deseq_results_df,
      file=filename,
      col.names=T,
      row.names=F
    )
  }
}

getMultivariateDeseqResults = function(
    lowest_rank,
    pvalue_cutoff,
    padj_cutoff,
    design
  )
{

  im_here = sprintf("In getMultivariateDeseqResults with: \n
                    \tlowest_rank=%s \n
                    \tpvalue_cutoff=%s \n
                    \tpadj_cutoff=%s\n
                    \tdesign=%s",
                    lowest_rank, pvalue_cutoff, padj_cutoff,
                    paste(as.character(design), collapse="")
                    )
  writeLines(im_here)

  ### Utility vectors for reused data
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  deseq_results_cols = c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj")
  deseq_keep_cols = c("log2FoldChange", "pvalue", "padj")

  lowest_rank_index = match(lowest_rank, all_ranks)
  ranks_to_keep = all_ranks[1:lowest_rank_index]

  ### Get the deseq results and auxiliary data
  deseq_data = collectDeseqData(
    lowest_rank=lowest_rank,
    pvalue_cutoff=pvalue_cutoff,
    padj_cutoff=padj_cutoff,
    design=design
  )

  ### The deseq object
  deseq_obj = deseq_data$deseq_obj

  ### The results names for contrasts, for example Gender_M_vs_F
  results_names = resultsNames(deseq_obj)

  ### Create a list where the names are the simple covariate names
  ### and the values are the results_names
  cov_name_list = sapply(results_names[-1],
   function(x)
   {
     ### Split off the covariate name.
     split_result_name = strsplit(x, '_')[[1]]
     print(split_result_name)
     print(length(split_result_name))
     if (length(split_result_name)==1)
     {
       print(x)
       names(x) = x
     } else
     {
       ### Usually, this requires removing
       ### the last 3 elements of the split string which are
       ### ['M','vs','F'] for example for Gender_M_vs_F.
       ### We need to be careful because variables without levels
       ### such as continuous variables show up as just the name of
       ### the covariate. To determine where to cut off the list of
       ### split characters, we first find the location of 'vs'.
       # vs_index = match('vs', split_result_name)
       # print(vs_index)
       ### Then we keep only up to that index - 1 to
       ### also drop the two list items before 'vs' which are 'F' and '_'
       ### Finally, we put what is left back together with a '_'
       split_result_name = split_result_name %>% head(-3)
       print(split_result_name)
       reassembled_name = paste0(split_result_name, collapse='_')
       print(reassembled_name)
       ### Set the name of the value to the covariate name
       names(x) = reassembled_name
     }

     return(x)
   },
   USE.NAMES=F,
   simplify=T
  )

  print(cov_name_list)

  ### This is going to create a list where the names are the
  ### covariate names, and each value is a dataframe of deseq
  ### result stats for that covariate.

  ### initialize empty list
  results_list = list()

  ### loop through covariate name list
  for (cov_name in names(cov_name_list))
  {
    print(sprintf('%s: %s', cov_name, cov_name_list[[cov_name]]))

    temp_results =
      ### Get the results for the current covariate by
      ### using its result_name
      results(deseq_obj, name=cov_name_list[[cov_name]]) %>%
      ### Coerce to dataframe
      as.data.frame() %>%
      ### Drop columns of stats we don't need
      select(one_of(deseq_keep_cols)) %>%
      ### Get the glommed taxa from the rownames
      ### It's important that these are in the right order
      ### for the join in the next step
      tibble::rownames_to_column('glommed_taxa') %>%
      ### Join to the aggregated counts to get some things we want:
      ### short_glommed_taxa and the separate taxa for each rank
      ### could be useful later
      inner_join(deseq_data$aggregated_counts, by='glommed_taxa') %>%
      ### But we don't need the actual raw counts, so drop the
      ### sampleID colunns
      select(-one_of(deseq_data$sampleIDs)) %>%
      ### Each result dataframe is going to have columns named
      ### `pvalue`, `padj`, and `log2FoldChange`. Use this trick
      ### to append the covariate name to the column names for these
      ### variables so we can distinguish them in the final combined dataframe
      rename_at(deseq_keep_cols, funs(paste0(.,'.', cov_name)))

    # print(colnames(temp_results))

    ### Add the results dataframe to the list
    results_list[[cov_name]] = temp_results
  }

  ### This is a bit tricky. We have a list of dataframes that contain the
  ### deseq stats results for each covariate. We actually want to join them
  ### by their shared columns (glommed_taxa, short_glommed_taxa, and taxa ranks).
  ### To generalize this for an unknown number of covariates, use the `Reduce`
  ### function to sequentially join the dataframes together.
  all_results = Reduce(
    function(...)
    {
      inner_join(..., by=c(all_ranks, 'glommed_taxa', 'short_glommed_taxa'))
    },
    results_list
  ) %>%
    ### Finally, reorder the columns so that the stats columns are all together
    select(glommed_taxa, short_glommed_taxa, one_of(ranks_to_keep), everything())

  return(all_results)

}

runDeseqFromTables = function(
    asv_table,
    taxonomy_table,
    metadata,
    lowest_rank,
    design
  )
{
  ### Utility vectors for reused data
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  deseq_results_cols = c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj")
  deseq_keep_cols = c("log2FoldChange", "pvalue", "padj")

  lowest_rank_index = match(lowest_rank, all_ranks)
  ranks_to_keep = all_ranks[1:lowest_rank_index]

  print("getting sample ids")
  sampleIDs = as.vector(metadata[['SampleID']])
  print(sampleIDs[1:3])

  ### Collect counts aggregated at a specific taxonomic rank
  print("getting aggregated counts")
  #
  # print("Setdiff")
  # print(colnames(asv_table))
  # print(metadata[['SampleID']])
  # print(setdiff(colnames(asv_table), metadata[['SampleID']]))

  aggregated_counts = getAggregatedTaxaRankCounts(
    taxonomy_table=taxonomy_table,
    asv_table=asv_table,
    metadata=metadata,
    lowest_rank=lowest_rank,
    clean=T
   )
  # print(dim(aggregated_counts))
  #
  # print("colnames aggregated counts")
  # print(colnames(aggregated_counts))
  # print("dim aggregated counts")
  # print(dim(aggregated_counts))

  print("getting taxa counts")
  taxa_counts =
    aggregated_counts %>%
    ### Remove zeros so log does not cause errors
    replace(.==0, 1) %>%
    ### Change tax glom to rownames
    tibble::remove_rownames() %>%
    tibble::column_to_rownames('glommed_taxa') %>%
    select(one_of(sampleIDs)) %>%
    data.frame()

  cat('dim taxa_counts:', dim(taxa_counts),"\n")
  cat('dim metadata:', dim(metadata),"\n")

  print("making deseq data set")
  dds <- DESeqDataSetFromMatrix(
    countData=taxa_counts,
    colData=metadata,
    design=design
  )

  print("making deseq object (running deseq)")
  deseq_obj = DESeq(
    dds,
    test='Wald',
    fitType='mean'
  )

  ### The results names for contrasts, for example Gender_M_vs_F
  results_names = resultsNames(deseq_obj)
  writeLines(c("results names:", results_names))

  ### Create a list where the names are the simple covariate names
  ### and the values are the results_names
  print("getting cov_name_list")
  print("splitting formula")
  split_formula = strsplit(as.character(design), '~')
  ### First element is always "". If there is a LHS, then
  ### element 2 is the LHS and element 3 is the RHS. Else
  ### element 2 is the RHS
  print(length(split_formula))
  if (length(split_formula)==3)
  {
    ### There is a left hand side
    lhs = split_formula[[2]] %>% trimws()
    rhs = split_formula[[3]] %>% trimws()
  } else
  {
    ### There is no left hand side
    lhs = ""
    rhs = split_formula[[2]]
  }
  rhs_pieces = strsplit(rhs, '+', fixed=T)[[1]] %>% trimws()
  cat('lhs:', lhs, 'rhs pices:', rhs_pieces, '\n')

  ### This creates a list where the names are the actual covariate names
  ### and the values are the output of resutsNames(dds)
  covariate_names = rhs_pieces
  cov_name_list = list()
  for (cov_name in covariate_names)
  {
    cov_name_regex = paste0(cov_name, '_', '*')
    name_index = which(grepl(cov_name_regex, results_names[-1]))
    cov_name_list[cov_name] = results_names[name_index+1]
  }
  print(cov_name_list)

  ### This is going to create a list where the names are the
  ### covariate names, and each value is a dataframe of deseq
  ### result stats for that covariate.

  ### This time try doing it using the covariate names and levels
  ### taken from the formula. But will al

  ### initialize empty list
  results_list = list()
  ### loop through covariate name list
  print("getting results_list")
  for (cov_name in names(cov_name_list))
  {
    print(sprintf('%s: %s', cov_name, cov_name_list[[cov_name]]))

    temp_results =
      ### Get the results for the current covariate by
      ### using its result_name
      results(deseq_obj, name=cov_name_list[[cov_name]]) %>%
      ### Coerce to dataframe
      as.data.frame() %>%
      ### Drop columns of stats we don't need
      select(one_of(deseq_keep_cols)) %>%
      ### Get the glommed taxa from the rownames
      ### It's important that these are in the right order
      ### for the join in the next step
      tibble::rownames_to_column('glommed_taxa') %>%
      ### Join to the aggregated counts to get some things we want:
      ### short_glommed_taxa and the separate taxa for each rank
      ### could be useful later
      inner_join(aggregated_counts, by='glommed_taxa') %>%
      ### But we don't need the actual raw counts, so drop the
      ### sampleID colunns
      select(-one_of(sampleIDs)) %>%
      ### Each result dataframe is going to have columns named
      ### `pvalue`, `padj`, and `log2FoldChange`. Use this trick
      ### to append the covariate name to the column names for these
      ### variables so we can distinguish them in the final combined dataframe
      rename_at(deseq_keep_cols, funs(paste0(.,'.', cov_name)))

    print(colnames(temp_results))

    ### Add the results dataframe to the list
    results_list[[cov_name]] = temp_results
  }

  print(names(results_list))

  ### This is a bit tricky. We have a list of dataframes that contain the
  ### deseq stats results for each covariate. We actually want to join them
  ### by their shared columns (glommed_taxa, short_glommed_taxa, and taxa ranks).
  ### To generalize this for an unknown number of covariates, use the `Reduce`
  ### function to sequentially join the dataframes together.
  print("assembling deseq_results")
  deseq_results = Reduce(
    function(...)
    {
      inner_join(..., by=c(all_ranks, 'glommed_taxa', 'short_glommed_taxa'))
    },
    results_list
    ) %>%
    ### Finally, reorder the columns so that the stats columns are all together
    select(glommed_taxa, short_glommed_taxa, one_of(ranks_to_keep), everything())

  return(list(
    deseq_results=deseq_results,
    aggregated_counts=aggregated_counts,
    deseq_obj=deseq_obj,
    taxa_counts=taxa_counts,
    sampleIDs=sampleIDs
  ))
}


runDeseqFromTables_dev = function(
    asv_table,
    taxonomy_table,
    sample_data,
    variables,
    lowest_rank='Genus',
    relative_abundance_cutoff = 0.002,
    prevalence_cutoff = 0.1,
    include_covariates='All'
  )
{

  print(sprintf('**** LOWEST RANK: %s *****', lowest_rank))

  ### Data for testing
  # asv_table = asv_table
  # taxonomy_table = taxonomy_table
  # sample_data = sample_data %>% filter(!!as.name(varname) %in% c(var$case, var$control))
  # variables = varname
  # lowest_rank = 'Genus'
  # relative_abundance_cutoff = 0.002
  # prevalence_cutoff = 0.1
  # include_covariates = 'All'



  sample_names = sample_data$SampleName

  # sample_data = setFactorsLevels(
  #   sample_data,
  #   variables
  # )

  # taxa_counts = getFilteredTaxaCounts(
  #   asv_table,
  #   taxonomy_table,
  #   sample_data,
  #   lowest_rank=lowest_rank,
  #   relative_abundance_cutoff=relative_abundance_cutoff,
  #   prevalence_cutoff=prevalence_cutoff,
  #   id_col="SampleName",
  #   normalize=F
  # )

  taxa_counts = getFilteredTaxaCountsDev(
    asv_table=asv_table,
    taxonomy_table=taxonomy_table,
    sample_data=sample_data,
    cluster_by=lowest_rank,
    relative_abundance_cutoff=relative_abundance_cutoff,
    prevalence_cutoff=prevalence_cutoff,
    min_count_cutoff=0,
    filter_by='Taxa',
    clean_taxa=T,  ### remove NAs in lowest rank
    n_max_by_mean=F,
    id_col="SampleName", ### metadata column that containes the unique sample IDs
    add_glommed_names=T,
    normalize=F
  )

  taxa_counts %<>%
    mutate_at(sample_names, list(function(x){x = x + 1}))

  filtered_taxa = taxa_counts$short_glommed_taxa

  design_formula =
    variables %>%
    # names() %>%
    paste(., collapse=' + ') %>%
    paste('~', .) %>%
    as.formula()

  print(design_formula)

  dds = DESeqDataSetFromMatrix(
    countData=
      taxa_counts %>%
      select(sample_names, short_glommed_taxa) %>%
      column_to_rownames('short_glommed_taxa'),
    colData=sample_data,
    design=design_formula
  )

  deseq_obj = DESeq(
    dds,
    test='Wald',
    fitType='mean'
  )

  contrast_names = resultsNames(deseq_obj)[-1]

  if (include_covariates != 'All')
  {
    contrast_names_to_use = sapply(
      include_covariates,
      function(x)
      {
        pattern = paste0('^', x)
        grep(pattern, contrast_names, value=T)
      },
      USE.NAMES = F
    )
  } else
  {
    contrast_names_to_use = contrast_names
  }

  deseq_results_df = data.frame(Taxon=filtered_taxa, stringsAsFactors=F)

  for (name in contrast_names_to_use)
  {
    print(name)

    name_results = results(
        deseq_obj,
        name=name,
        format='DataFrame'
      ) %>%
      data.frame(stringsAsFactors=F) %>%
      rownames_to_column('Taxon') %>%
      arrange(padj) %>%
      # rename(L2FC=log2FoldChange) %>%
      select(Taxon, padj, L2FC=log2FoldChange, pvalue, everything()) %>%
      rename_at(setdiff(names(.), 'Taxon'), function(x){paste0(name,'.',x)})

    deseq_results_df = inner_join(deseq_results_df, name_results, by='Taxon')
  }

  return(deseq_results_df)
}


runDeseqFromPathwayCounts = function(
  pathway_counts,
  sample_data,
  variables,
  include_covariates='All',
  pathway_colname="",
  cooks_cutoff=F,
  independent_filtering=F
)
{

  ### Data for testing
  # pathway_counts = ko_counts_table
  # sample_data = sample_data
  # variables = "CaseString"
  # include_covariates="All"
  # pathway_colname="function."


  sample_names = sample_data$SampleName

  design_formula =
    variables %>%
    # names() %>%
    paste(., collapse=' + ') %>%
    paste('~', .) %>%
    as.formula()

  print(design_formula)

  pathway_names = pathway_counts %>% pull(!!pathway_colname)

  dds = DESeqDataSetFromMatrix(
    countData=
      pathway_counts %>%
      select(sample_names, !!pathway_colname) %>%
      column_to_rownames(pathway_colname),
    colData=sample_data,
    design=design_formula
  )

  deseq_obj = DESeq(
    dds,
    test='Wald',
    fitType='mean'
  )

  contrast_names = resultsNames(deseq_obj)[-1]

  if (include_covariates != 'All')
  {
    contrast_names_to_use = sapply(
      include_covariates,
      function(x)
      {
        pattern = paste0('^', x)
        grep(pattern, contrast_names, value=T)
      },
      USE.NAMES = F
    )
  } else
  {
    contrast_names_to_use = contrast_names
  }

  deseq_results_df =
    data.frame(tempname=pathway_names, stringsAsFactors=F) %>%
    rename(!!pathway_colname:=tempname)

  for (name in contrast_names_to_use)
  {
    print(name)

    name_results = results(
      deseq_obj,
      name=name,
      format='DataFrame',
      cooksCutoff=cooks_cutoff,
      independentFiltering=independent_filtering
    ) %>%
      data.frame(stringsAsFactors=F) %>%
      rownames_to_column(pathway_colname) %>%
      arrange(padj) %>%
      # rename(L2FC=log2FoldChange) %>%
      select(!!pathway_colname, padj, L2FC=log2FoldChange, pvalue, everything()) %>%
      rename_at(setdiff(names(.), pathway_colname), function(x){paste0(name,'.',x)})

    deseq_results_df = inner_join(deseq_results_df, name_results, by=pathway_colname)
  }

  return(deseq_results_df)
}



getSignificantTaxaCounts2 = function(raw_counts, deseq_results_df, cutoff_expr
)
{
  counts_df =
    raw_counts %>%
    data.frame() %>%
    tibble::rownames_to_column('glommed_taxa')

  significant_glommed_taxa =
    deseq_results_df %>%
    filter(.data(cutoff_expr)) %>%
    pull(glommed_taxa)

  sig_taxa_counts =
    counts_df %>%
    filter(glommed_taxa %in% significant_glommed_taxa) %>%
    tibble::column_to_rownames('glommed_taxa') %>%
    t() %>%
    as.data.frame()
}


# getFilteredTaxaCounts = function(
#       asv_table,
#       taxonomy_table,
#       metadata,
#       lowest_rank=NA,
#       relative_abundance_cutoff,
#       prevalence_cutoff,
#       clean=T,
#       n_max_by_mean=F,
#       id_col="SampleName"
#     )
# {
#
#   print(sprintf("lowest_rank = %s", lowest_rank))
#   all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
#
#   if (is.na(lowest_rank))
#   {
#     print("using ASVs")
#     use_taxa = F
#     lowest_rank = 'Genus'
#   } else
#   {
#     print("using taxa")
#     use_taxa = T
#   }
#
#   lowest_rank_index = match(lowest_rank, all_ranks)
#   ### get just the ranks that will be used in
#   ### 1. The glommed name
#   ### 2. The step of aggregating sums
#   ranks_to_glom = all_ranks[1:lowest_rank_index]
#   short_ranks = all_ranks[c(1,lowest_rank_index)]
#   print("ranks to glom")
#   print(ranks_to_glom)
#
#   sample_names = as.character(metadata[,id_col])
#   print(sprintf("length sample_names = %d", length(sample_names)))
#
#   if (clean && use_taxa)
#   {
#     ### Remove any taxa that are NA at the lowest level.
#     ### No point in having ASVs aggregated to the same genus
#     ### if that genus is NA.
#     clean_taxonomy_table =
#       taxonomy_table %>%
#       # filter(sprintf("!is.na(%s)", lowest_rank))
#       filter(!is.na(!!sym(lowest_rank)))
#
#     print("dim clean tax table")
#     print(dim(clean_taxonomy_table))
#   }else
#   {
#     clean_taxonomy_table = taxonomy_table
#   }
#
#   print("creating asv taxa counts table")
#   counts = inner_join(asv_table, clean_taxonomy_table, by="ASVs")
#
#   if (use_taxa)
#   {
#     print("Aggregating by ranks")
#     ### Aggregate counts by ranks being kept
#     counts =
#       counts %>%
#       ### Drop the ASVs
#       select(!!c(sample_names, ranks_to_glom)) %>%
#       ### Group and sum
#       group_by(.dots=ranks_to_glom) %>%
#       summarize_all(sum) %>%
#       ungroup()
#   }
#
#   print("glomming taxa names")
#   print(dim(counts))
#
#   glommed_taxa_counts =
#     counts %>%
#     ### Glom taxa ranks together for additinoal column
#     mutate(glommed_taxa = unite(., 'q', ranks_to_glom)$q) %>%
#     ### Fix hyphens in taxa names so they don't mess up column names later
#     mutate(glommed_taxa = gsub('-', '_dash_', glommed_taxa)) %>%
#     ### Fix slashes in taxa names so they don't mess up column names later
#     mutate(glommed_taxa = gsub('/', '_slash_', glommed_taxa)) %>%
#     ### Select again so glommed_taxa is first row (better way?)
#     ### Could glom earlier and then group by ranks and glommed_taxa
#     select(one_of('glommed_taxa', sample_names, ranks_to_glom), everything())  %>%
#     mutate(short_glommed_taxa = unite(., 'q', short_ranks, sep="_")$q) %>%
#     mutate(short_glommed_taxa = make.unique(short_glommed_taxa)) %>%
#     ### Convert to dataframe (instead of tibble)
#     data.frame()
#
#   print("dim taxa counts")
#   print(dim(glommed_taxa_counts))
#
#   print("Filtering taxa")
#   print("Relative Abundance Filter")
#
#   relative_abundance_filtered_counts =
#     glommed_taxa_counts %>%
#     ### Set counts with relative abundance less than cutoff to
#     mutate_if(is.numeric, list(~ifelse(./sum(.)<relative_abundance_cutoff, 0, .))) %>%
#     # mutate_at(.vars=sample_names, list(.=~ifelse(./sum(.)<relative_abundance_cutoff, 0, .))) %>%
#     ### Add a rowsum column
#     mutate(rowsum=apply(select_if(., is.numeric), 1, sum)) %>%
#     ### Drop rows that now have all zero counts
#     filter(rowsum>0) %>%
#     ### Drop the rowsum column
#     select(-rowsum)
#
#   print("current dim")
#   print(dim(relative_abundance_filtered_counts))
#
#   num_taxa_frequency_dropped = dim(glommed_taxa_counts)[1] - dim(relative_abundance_filtered_counts)[1]
#   print(paste('Num count filtered taxa dropped:', num_taxa_frequency_dropped))
#
#   print("Prevalence Filter")
#   prevalence_filtered_counts =
#     relative_abundance_filtered_counts %>%
#     mutate(
#       ### Get the number of numeric columns
#       num_numeric=length(select_if(., is.numeric)),
#       ### Get the number of zeros
#       num_nonzero_numeric=rowSums(select_if(.,is.numeric)!=0),
#       ### Fraction that are zero
#       fract_nonzero=num_nonzero_numeric/num_numeric
#     ) %>%
#     ### Keep only features appearing in more than 10% of samples
#     filter(fract_nonzero>=prevalence_cutoff) %>%
#     ### Drop temporary columns
#     select(-num_numeric, -num_nonzero_numeric, -fract_nonzero)
#   num_prevalence_filtered_taxa = dim(prevalence_filtered_counts)[1]
#
#   num_taxa_prevalence_dropped =
#     dim(relative_abundance_filtered_counts)[1] - dim(prevalence_filtered_counts)[1]
#
#   print(paste0('Num prevalence filtered taxa dropped: ', num_taxa_prevalence_dropped))
#
#   if (n_max_by_mean != F)
#   {
#    ### Keep only the n_max_by_mean features
#     prevalence_filtered_counts =
#       prevalence_filtered_counts %>%
#       ### Create a column of row means
#       mutate(mean=rowMeans(
#         select(., sample_names)/sum(select(., sample_names))
#       )) %>%
#       ### Order by the means (ascending)
#       arrange(mean) %>%
#       ### Take the highest ones (n_max_by_mean)
#       tail(n_max_by_mean) %>%
#       ### Drop the mean column
#       select(-mean)
#   }
#
#   return(prevalence_filtered_counts %>% data.frame())
#
# }

