library(tidyverse)
library(magrittr)

getAggregatedTaxaRankCounts = function(
  taxonomy_table, # Table with ASVs and taxa ranks as columns, taxa names as values
  asv_table, # Table with ASVs and sample IDs as columns, counts as values
  metadata,
  lowest_rank="Genus", # Lowest taxa rank to group by and aggregate on
  clean=T # remove taxa where lowest rank value is NA
)
{
  
  ### Result: table with columns for taxa names, agglommerated taxa names, and counts 
  ### for each sample.
  
  
  im_here = sprintf("in getAggregatedTaxaRankCounts with: \n
                    \ttaxonomy_table=%s \n
                    \tmetadata=%s \n
                    \tlowest_rank=%s \n
                    \tasv_table=%s \n
                    \tclean=%s \n",
                    paste(dim(taxonomy_table), collapse=" "),
                    paste(dim(metadata), collapse=" "),
                    lowest_rank,
                    paste(dim(asv_table), collapse=" "),
                    clean
  )
  writeLines(im_here)
  
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  print(lowest_rank)
  lowest_rank_index = match(lowest_rank, all_ranks)
  ranks_to_glom = all_ranks[1:lowest_rank_index]
  print("ranks_to_glom")
  print(ranks_to_glom)
  
  sampleIDs = as.vector(metadata$SampleID)
  
  if (clean)
  {
    taxonomy_table = 
      taxonomy_table %>%
      # filter(sprintf("!is.na(%s)", lowest_rank))
      filter(!is.na(!!sym(lowest_rank)))
  }
  
  # print("Setdiff")
  # print(colnames(asv_table))
  # print(metadata[['SampleID']])
  # print(setdiff(colnames(asv_table), metadata[['SampleID']]))
  
  
  print("creating tax abundance table")
  tax_abundance_table = 
    ### Join asv_table and taxonomy_table on ASVs to get sample abundances and rank names
    asv_table %>% 
    inner_join(taxonomy_table, by='ASVs')%>% 
    ### Drop everything but the sampleIDs and requested taxa
    select(one_of(!!sampleIDs), one_of(ranks_to_glom))%>%
    ### Sum all the counts grouping by the axa (effectively genus)
    group_by(.dots=ranks_to_glom) %>%
    summarize_all(sum)%>%
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
    data.frame() %>%
    mutate(short_glommed_taxa=paste0(Phylum, "_", !!as.name(lowest_rank))) %>%
    mutate(short_glommed_taxa = make.unique(short_glommed_taxa))
  
  # print(dim(tax_abundance_table))
  
  return(tax_abundance_table)
}


getMasterTable = function(
  taxa_counts,
  metadata,
  key_column='SampleID',
  taxa_colnames='short_glommed_taxa'
)
{
  ### Table combining agglomerated taxa counts and metadata. For each
  ### sample, columns include metadata followed by agglomerated taxa names.
  ### Values are counts.
  
  # taxa_counts = taxa_abundance
  # metadata = sample_metadata
  # key_column = 'SampleName'
  # taxa_colnames = 'short_glommed_taxa'
  
  join_keys = metadata[[key_column]]
  taxa_columns = taxa_counts[[taxa_colnames]]
  
  master_table = 
    taxa_counts %>%
    select(join_keys, taxa_colnames) %>%
    ### Add rownames which will become colnames (headers) when transposed
    column_to_rownames(taxa_colnames) %>% 
    ### Transpose and coerce back to dataframe
    t() %>% 
    data.frame() %>% 
    remove_rownames() %>% 
    mutate(!!key_column := join_keys) %>%
    inner_join(metadata, by=key_column) %>% 
    select(key_column, everything())
  
  return(master_table)
}


### dataframe with cols for SampleID, taxa, and metadata
plotTaxaCounts = function(
  master_table,
  select_taxa,
  taxa_rank,
  formula,
  pvalue_cutoff=0.1,
  padj_cutoff=0.2,
  normalize=T
)
{
  ### Creates a faceted set of bar plots of the taxa abundance in each sample.
  ### Accepts a formula to use for faceting. Right now, the LHS has to be Taxa,
  ### and only one feature on the RHS.
  
  # print('head counts')
  # print(head(counts))
  
  print("checking normalize")
  if (normalize)
  {
    print("normalizing counts for select")
    master_table = 
      master_table %>% 
      mutate_at(vars(select_taxa), funs(./sum(.)))
  }
  
  print("splitting formula")
  split_formula = as.character(formula)
  lhs = split_formula[2] %>% trimws()
  rhs = split_formula[3]
  rhs_pieces = strsplit(rhs, '+', fixed=T)[[1]] %>% trimws()
  cat('lhs:', lhs, 'rhs pices:', rhs_pieces, '\n')
  
  kept_columns = c('SampleID', rhs_pieces)
  print("kept columns")
  print(kept_columns)
  
  # gathered_data =  
  #   counts %>%
  #   select(glommed_taxa, one_of(kept_columns)) %>%
  # gather(key='Taxa', value='Counts', -kept_columns, factor_key=T) 
  
  gathered_data =  
    master_table %>%
    select(c(!!select_taxa, !!kept_columns)) %>%
    gather(key='Taxa', value='Counts', -kept_columns, factor_key=T) %>%
    mutate(Taxa = shortenTaxaNames(Taxa))
  
  # print(head(gathered_data))
  
  print("dim gathered data")
  print(dim(gathered_data))
  
  # gathered_data$Taxa = sapply(gathered_data$Taxa, shortenTaxaName)
  # print(head(gathered_data))
  
  plot_title = 
    sprintf('%s level relative abundance for p<=%s, padj<=%s significant taxa', taxa_rank, pvalue_cutoff, padj_cutoff)
  print(plot_title)
  
  plot_obj = ggplot(gathered_data, aes(x=SampleID, y=Counts)) + 
    geom_bar(stat='identity') + 
    facet_grid(formula, scale='free') +
    theme(strip.text.y=element_text(angle=0, hjust=1, size=8)) +
    ggtitle(plot_title)
  
  return(plot_obj)
}

plotTaxaCounts2 = function(
  taxa_counts,
  metadata,
  deseq_results_df,
  taxa_rank,
  formula,
  pvalue_cutoff=0.1,
  padj_cutoff=0.2,
  normalize=T
)
{
  ### Creates a faceted set of bar plots of the taxa abundance in each sample.
  ### Accepts a formula to use for faceting. Right now, the LHS has to be Taxa,
  ### and only one feature on the RHS.
  
  # print('head counts')
  # print(head(counts))
  
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  
  sampleIDs = metadata %>% pull(SampleID) %>% as.vector()
  
  if (normalize)
  {
    print("normalizing counts for select")
    taxa_counts = 
      taxa_counts %>% 
      mutate_if(is.numeric, funs(./sum(.)))
  }
  
  select_taxa = 
    deseq_results_df %>%
    filter(pvalue<pvalue_cutoff, padj<padj_cutoff) %>%
    pull(glommed_taxa) %>%
    as.vector()
  
  master_table = 
    getMasterTable2(taxa_counts, metadata) %>%
    select(!!colnames(metadata), !!select_taxa)
  
  print("splitting formula")
  split_formula = as.character(formula)
  lhs = split_formula[2] %>% trimws()
  rhs = split_formula[3]
  rhs_pieces = strsplit(rhs, '+', fixed=T)[[1]] %>% trimws()
  cat('lhs:', lhs, 'rhs pices:', rhs_pieces, '\n')
  
  kept_columns = c('SampleID', rhs_pieces)
  print("kept columns")
  print(kept_columns)
  
  # gathered_data =  
  #   counts %>%
  #   select(glommed_taxa, one_of(kept_columns)) %>%
  # gather(key='Taxa', value='Counts', -kept_columns, factor_key=T) 
  
  gathered_data =  
    master_table %>%
    select(one_of(select_taxa, kept_columns)) %>%
    gather(key='Taxa', value='Counts', -kept_columns, factor_key=T) %>%
    mutate(Taxa = shortenTaxaNames(Taxa))
  
  # print(head(gathered_data))
  
  print("dim gathered data")
  print(dim(gathered_data))
  
  # gathered_data$Taxa = sapply(gathered_data$Taxa, shortenTaxaName)
  # print(head(gathered_data))
  
  plot_title = 
    sprintf('%s level relative abundance for p<=%s, padj<=%s significant taxa', taxa_rank, pvalue_cutoff, padj_cutoff)
  print(plot_title)
  
  plot_obj = ggplot(gathered_data, aes(x=SampleID, y=Counts)) + 
    geom_bar(stat='identity') + 
    facet_grid(formula, scale='free') +
    ggtitle(plot_title) +
    theme(
      strip.text.y=element_text(angle=0, hjust=1, size=8),
      plot.title = element_text(hjust=0, size=8)
    )
  
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

clearMem = function(current_file="")
{
  # unlink(current_file', recursive = TRUE)
  for (i in 1:10){print(gc(full=T))}
}


addGlommedTaxaNames = function(
  feature_abundance,
  lowest_rank='Genus'
)
{
  print("in addGlommedTaxaNames")
  
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  lowest_rank_index = match(lowest_rank, all_ranks)
  ### get just the ranks that will be used in
  ### 1. The glommed name
  ### 2. The step of aggregating sums
  ranks_to_glom = all_ranks[1:lowest_rank_index]
  # print("ranks to glom")
  # print(ranks_to_glom)
  
  # print("glomming taxa names")
  # print(dim(feature_abundance))
  
  glommed_taxa_counts =
    feature_abundance %>%
    ### Glom taxa ranks together for additinoal column
    unite('glommed_taxa', ranks_to_glom, sep='_', remove=F) %>%
    unite('short_glommed_taxa', c('Phylum', lowest_rank), sep="_", remove=F) %>%
    mutate(short_glommed_taxa = make.unique(short_glommed_taxa)) %>%
    ### Fix hyphens in taxa names so they don't mess up column names later
    mutate(
      glommed_taxa = gsub('-', '_dash_', glommed_taxa),
      short_glommed_taxa = gsub('-', '_dash_', short_glommed_taxa)
    ) %>%
    # ### Fix slashes in taxa names so they don't mess up column names later
    mutate(
      glommed_taxa = gsub('/', '_slash_', glommed_taxa),
      short_glommed_taxa = gsub('/', '_slash_', short_glommed_taxa)
    ) %>%
    ### Select again so glommed_taxa is first row (better way?)
    ### Could glom earlier and then group by ranks and glommed_taxa
    select(c('glommed_taxa', 'short_glommed_taxa'), everything(), ranks_to_glom)  %>%
    ### Convert to dataframe (instead of tibble)
    data.frame()
  
  # print("dim taxa counts")
  # print(dim(glommed_taxa_counts))
  
  return(glommed_taxa_counts)
}

applyPrevalenceFilter = function(
  feature_abundance,
  prevalence_cutoff
)
{
  print("in applyPrevalenceFilter")
  prevalence_filtered_abundance =
    feature_abundance %>%
    mutate(
      ### Get the number of numeric columns
      num_numeric=length(select_if(., is.numeric)),
      ### Get the number of zeros
      num_nonzero_numeric=rowSums(select_if(.,is.numeric)!=0),
      ### Fraction that are zero
      fract_nonzero=num_nonzero_numeric/num_numeric
    ) %>%
    ### Keep only features appearing in more than 10% of samples
    filter(fract_nonzero>=prevalence_cutoff)
  # %>%
  #   ### Drop temporary columns
  #   select(-num_numeric, -num_nonzero_numeric, -fract_nonzero)
  # print("dim prevalence filtered abundance")
  # print(dim(prevalence_filtered_abundance))

  num_features_prevalence_dropped =
    dim(feature_abundance)[1] - dim(prevalence_filtered_abundance)[1]

  print(paste0('Num prevalence filtered features dropped: ', num_features_prevalence_dropped))

  return(prevalence_filtered_abundance)
}

applyRelativeAbundanceFilter = function(
  feature_abundance,
  relative_abundance_cutoff
)
{
  print("in applyRelativeAbundanceFilter")
  
  relative_abundance_filtered_counts = 
    feature_abundance %>%
    ### Set counts with relative abundance less than cutoff to 0
    mutate_if(is.numeric, list(~ifelse(./sum(.)<relative_abundance_cutoff, 0, .))) %>%
    # mutate_at(.vars=sample_names, list(~ifelse(./sum(.)<relative_abundance_cutoff, 0, .))) %>%
    ### Add a rowsum column
    mutate(rowsum=apply(select_if(., is.numeric), 1, sum)) %>%
    ### Drop rows that now have all zero counts
    filter(rowsum>0) %>%
    ### Drop the rowsum column
    select(-rowsum)
  
  # print("dim relative abundance filtered counts")
  # print(dim(relative_abundance_filtered_counts))
  
  num_taxa_frequency_dropped = dim(feature_abundance)[1] - dim(relative_abundance_filtered_counts)[1]
  print(paste('Num count filtered taxa dropped:', num_taxa_frequency_dropped))
  
  return(relative_abundance_filtered_counts)
}

getRelativeAbundance = function(counts)
{
  print("in getRelativeAbundance")
  
  relative_abundance = 
    counts %>%
    mutate_if(is.numeric, list(~ ./sum(.)))
  
  return(relative_abundance)
}


getFilteredTaxaCounts = function(
  asv_table,
  taxonomy_table,
  metadata,
  lowest_rank=NA,
  relative_abundance_cutoff=1,
  prevalence_cutoff=1,
  clean=T,  ### remove NAs in lowest rank
  n_max_by_mean=F,
  id_col="SampleName", ### metadata column that containes the unique sample IDs
  add_glommed_names=T,
  normalize=F
)
{
  print("in getFilteredTaxaCounts")
  
  # print(sprintf("lowest_rank = %s", lowest_rank))
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')
  
  if (is.na(lowest_rank))
  {
    # print("using ASVs")
    use_taxa = F
    lowest_rank = 'Genus'
  } else
  {
    # print("using taxa")
    use_taxa = T
  }
  
  lowest_rank_index = match(lowest_rank, all_ranks)
  ### get just the ranks that will be used in
  ### 1. The glommed name
  ### 2. The step of aggregating sums
  ranks_to_glom = all_ranks[1:lowest_rank_index]
  short_ranks = all_ranks[c(1,lowest_rank_index)]
  # print("ranks to glom")
  # print(ranks_to_glom)
  
  sample_names = as.character(metadata[,id_col])
  # print(sprintf("length sample_names = %d", length(sample_names)))
  
  if (clean && use_taxa)
  {
    ### Remove any taxa that are NA at the lowest level.
    ### No point in having ASVs aggregated to the same genus
    ### if that genus is NA.
    clean_taxonomy_table =
      taxonomy_table %>%
      # filter(sprintf("!is.na(%s)", lowest_rank))
      filter(!is.na(!!sym(lowest_rank)))
    
    # print("dim clean tax table")
    # print(dim(clean_taxonomy_table))
  }else
  {
    clean_taxonomy_table = taxonomy_table
  }
  
  # print("creating asv taxa counts table")
  counts = inner_join(asv_table, clean_taxonomy_table, by="ASVs")
  
  if (use_taxa)
  {
    # print("Aggregating by ranks")
    ### Aggregate counts by ranks being kept
    counts = 
      counts %>%
      ### Drop the ASVs
      select(!!c(sample_names, ranks_to_glom)) %>%
      ### Group and sum
      group_by(.dots=ranks_to_glom) %>%
      summarize_all(sum) %>%
      ungroup()
  }
  
  # print("Filtering taxa")
  # print("Relative Abundance Filter")
  
  filtered_counts =
    counts %>%
    applyRelativeAbundanceFilter(relative_abundance_cutoff) %>%
    applyPrevalenceFilter(prevalence_cutoff)
  
  # # print("current dim")
  # print(dim(filtered_counts))
  
  if (normalize)
  {
    filtered_counts = getRelativeAbundance(filtered_counts)
  }
  
  if (n_max_by_mean != F)
  {
    ### Keep only the n_max_by_mean features 
    filtered_counts = 
      filtered_counts %>%
      ### Create a column of row means
      mutate(mean=rowMeans(
        select(., sample_names)/sum(select(., sample_names))
      )) %>%
      ### Order by the means (ascending)
      arrange(mean) %>%
      ### Take the highest ones (n_max_by_mean)
      tail(n_max_by_mean) %>%
      ### Drop the mean column
      select(-mean)
  }
  
  if (add_glommed_names)
  {
    filtered_counts = addGlommedTaxaNames(filtered_counts)
  }
  
  return(filtered_counts %>% data.frame())
  
}

getRelAbund = function(
  counts,
  use_cols=c()
)
{
  ### Normalize columns by column sum
  
  if ( length(use_cols) == 0 )
  {
    use_cols = colnames(df)
  }
  
  relabund =
    counts %>%
    mutate_if(is.numeric, funs(./sum(.))) %>%
    data.frame()
  
  return(relabund)
}



getTopNbyRowMean = function(
  data_table,
  use_samples=c(),
  num_top_features
)
{
  ### Not used I don't think. Seems superfluous
  if (length(use_samples)==0)
  {
    use_samples = everything()
  }
  
  top_N =
    data_table %>%
    mutate_at(.vars=use_samples, funs(./sum(.))) %>%
    mutate(RowSums = rowSums(select(.,use_samples))) %>%
    arrange(RowSums) %>%
    tail(n=num_top_features) %>%
    select(-RowSums)
  
  return(top_N)
}


getDataCombinedWithMetadata = function(
  data,
  metadata,
  data_pivot_column,
  metadata_pivot_column
)
{
  
  ### Table with subjects or samples as rows and columns are taxa as well
  ### as the metadata. This allows stats stuff with formulas containing
  ### a taxon and a metadata column (e.g. Case, SNP, Gender)
  
  ### Data for testing
  # data = taxa_abundance
  # metadata = sample_metadata
  # observation_pivot_column='short_glommed_taxa'
  # metadata_pivot_column = 'SampleName'
  
  data_and_metadata =
    data %>%
    ### Stash short_glommed_taxa as rownames before transpose
    ### They will become
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(data_pivot_column) %>%
    ### Transpose to put samples in rows, taxa and metadata in cols
    t() %>%
    ### Coerce back to dataframe for further manipulation
    data.frame() %>%
    ### The subject IDs were the column names. After the transpose
    ### they became row names. To be "tidy" we need them in an actual column
    tibble::rownames_to_column(metadata_pivot_column) %>%
    ### Join this table to the sample_metadata table
    left_join(metadata, by=metadata_pivot_column)
}

writeData = function(
  data,
  project,
  dir,
  name
)
{
  filename = file.path(dir, paste0(project, name, '.tsv'))
  write.table(
    data,
    file=filename,
    sep='\t',
    quote=F
  )
  
  filename = file.path(dir, paste0(project, name, '.xlsx'))
  write.xlsx(
    data,
    file=filename,
    sep='\t',
    quote=F
  )
}


