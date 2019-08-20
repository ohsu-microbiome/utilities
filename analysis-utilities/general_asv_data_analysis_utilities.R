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
  print("ranks to glom")
  print(ranks_to_glom)

  print("glomming taxa names")
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

  # metadata = sample_data %>% filter(IGA=='AllBac')
  # lowest_rank = 'asv'
  # relative_abundance_cutoff = 0.002
  # prevalence_cutoff = 0
  # clean=T
  # id_col='SubjectID'
  # add_glommed_names=T
  # normalize=T
  # n_max_by_mean=F

  # print(sprintf("lowest_rank = %s", lowest_rank))
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')

  if (is.na(lowest_rank) | tolower(lowest_rank)=='asv' )
  {
    print("using ASVs")
    use_taxa = F
    lowest_rank = 'Genus'
  } else
  {
    lowest_rank = tools::toTitleCase(lowest_rank)
    print("using taxa")
    use_taxa = T
  }

  lowest_rank_index = match(lowest_rank, all_ranks)
  ### get just the ranks that will be used in
  ### 1. The glommed name
  ### 2. The step of aggregating sums
  ranks_to_glom = all_ranks[1:lowest_rank_index]
  short_ranks = all_ranks[c(1,lowest_rank_index)]
  print("ranks to glom")
  print(ranks_to_glom)

  sample_names = as.character(metadata[,id_col])
  # print(sprintf("length sample_names = %d", length(sample_names)))

  if (clean && use_taxa)
  {
    ### Remove any taxa that are NA at the lowest level.
    ### No point in having ASVs aggregated to the same genus
    ### if that genus is NA.
    clean_taxonomy_table =
      taxonomy_table %>%
      filter(Kingdom == 'Bacteria') %>%
      filter(!is.na(Phylum)) %>%
      # filter(sprintf("!is.na(%s)", lowest_rank))
      filter(!is.na(!!sym(lowest_rank)))

    print("dim clean tax table")
    print(dim(clean_taxonomy_table))
  }else
  {
    clean_taxonomy_table = taxonomy_table
  }

  print("creating asv taxa counts table")
  counts = inner_join(asv_table, clean_taxonomy_table, by="ASVs")

  if (use_taxa)
  {
    # print("Aggregating by ranks")
    ### Aggregate counts by ranks being kept
    counts =
      counts %>%
      ### Drop the ASVs
      select(!!c(sample_names, ranks_to_glom)) %>%
      # select(-ASVs) %>%
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
    filtered_counts = addGlommedTaxaNames(filtered_counts, lowest_rank)
  }

  return(filtered_counts %>% data.frame())

}

getFilteredTaxaCountsDev = function(
  asv_table,
  taxonomy_table,
  metadata,
  cluster_by=NA,
  relative_abundance_cutoff=0,
  prevalence_cutoff=0,
  min_count_cutoff=0,
  filter_by='Taxa',
  clean_taxa=T,  ### remove NAs in lowest rank
  n_max_by_mean=F,
  id_col="SampleName", ### metadata column that containes the unique sample IDs
  add_glommed_names=T,
  normalize=F
)
{
  print("in getFilteredTaxaCounts")

  # print(sprintf("lowest_rank = %s", lowest_rank))
  all_ranks = c('Phylum', 'Class', 'Order', 'Family', 'Genus')

  lowest_rank_index = match(lowest_rank, all_ranks)
  ### get just the ranks that will be used in
  ### 1. The glommed name
  ### 2. The step of aggregating sums
  ranks_to_glom = all_ranks[1:lowest_rank_index]
  short_ranks = all_ranks[c(1,lowest_rank_index)]
  print("ranks to glom")
  print(ranks_to_glom)

  sample_names = as.character(metadata[,id_col])
  # print(sprintf("length sample_names = %d", length(sample_names)))

  print("creating asv taxa counts table")
  counts = inner_join(asv_table, clean_taxonomy_table, by="ASVs")

  if (!is.na(cluster_by) & toupper(cluster_by) != 'ASV')
  {
    # print("Aggregating by ranks")
    ### Aggregate counts by ranks being kept
    counts =
      counts %>%
      ### Drop the ASVs
      select(!!c(sample_names, ranks_to_glom)) %>%
      # select(-ASVs) %>%
      ### Group and sum
      group_by(.dots=ranks_to_glom) %>%
      summarize_all(sum) %>%
      ungroup()
  }

  # print("Filtering taxa")
  # print("Relative Abundance Filter")

  filtered_counts =
    counts %>%
    applyMinCountFilter(min_count_cutoff) %>%
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
    filtered_counts = addGlommedTaxaNames(filtered_counts, lowest_rank)
  }

  if (clean_taxa)
  {
    ### Remove any taxa that are NA at the lowest level.
    ### No point in having ASVs aggregated to the same genus
    ### if that genus is NA.
    clean_taxonomy_table =
      taxonomy_table %>%
      # filter(sprintf("!is.na(%s)", lowest_rank))
      filter(!is.na(!!sym(cluster_by)))

    # print("dim clean tax table")
    # print(dim(clean_taxonomy_table))
  }else
  {
    clean_taxonomy_table = taxonomy_table
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


makeColDataPlot = function(
  master_table,
  columns,
  aesthetics, ### list(x_var='', color_var='', facet_var='')
  additional_title_text=''
)
{
  ### Aesthetics
  ### x_var: Which variable defines the groups along the x-axis
  ### color_var: which variable is used for different color groups (legend)
  ### facet_var: which variable is used to group the plots

  x_var = aesthetics$x_var
  color_var = aesthetics$color_var
  facet_var = aesthetics$facet_var
  cols_to_not_gather = c(x_var, color_var, facet_var)

  plt =
  master_table
  select(!!columns, !!cols_to_not_gather) %>%
  gather(key='column', value='value', -!!cols_to_not_gather) %>%
    ggplot(aes_string(
      x=x_var,
      y='value',
      color=color_var
    )) +
    geom_quasirandom(
      width=0.2,
      method='smiley',
      alpha=0.7,
      size=0.8,
      dodge.width=1
    ) +
    # geom_boxplot(alpha=0.1, fill='black', colour='black', size=0.5, varwidth=T, width=0.7) +
    geom_boxplot(
      aes_string(
        x=x_var,
        y='value',
        color=color_var
      ),
      alpha=0.1,
      # color='black',
      size=0.5,
      width=0.7
    )+
    ggtitle(paste('Plot:', color_var, '+', x_var, additional_title_text))

  if (facet_var != '')
  {
    plt = plt +
      facet_wrap(as.formula(paste('~',facet_var)), scales='free', shrink=F)

  }

  print(plt)

}


filterVarsBySampleThreshold = function(
  master_table, 
  threshold, 
  variable_list
  )
{
  new_variable_list = list()

  for (var in variable_list)
  {

    print(var)

    name = var$covariate_of_interest
    case = var$case %>% as.character()
    control = var$control %>% as.character()

    print(sprintf('name: %s, case: %s, control: %s', name, case, control))

    temp_master_table = 
      master_table %>% 
      filter(!!as.name(name) %in% c(case, control))

    ### Ignore if not in master table
    if (!(name %in% colnames(master_table)))
    {
      print("next")
      next
    }

    ### Make sure both levels present
    if ((temp_master_table %>% pull(name) %>% unique() %>% length()) < 2)
    {
      print("only one level. skipping...")
      next
    }

    ### to deal with continuous covariate such as Age
    if (case=="" & control=="")
    {
      ### Always allow
      min_samples = 9999
    } else
    {
      temp_master_table %>% pull(name) %>% table() %>% print()
      min_samples = temp_master_table %>% pull(name) %>% table() %>% min(.)
    }
    print(sprintf('min_samples (all levels): %d', min_samples))

    if (min_samples >= num_samples_threshold)
    {
      print("adding var")
      print(name)
      new_variable_list[[name]] = var
      # print(names(new_variable_list))
    }else
    {
      print(sprintf('%s has only %d samples', name, min_samples))
    }
  }


  return(new_variable_list)
}

filterVarsBySampleThreshold2 = function(
  master_table, 
  threshold, 
  variable_list,
  covariate=c()
)
{
  
  master_table = all_master_table
  threshold = 3
  variable_list = observational_variables
  covariate="CaseString"
  var = observational_variables$ARMS2rs10490924
  print(names(variable_list))
  
  new_variable_list = list()
  
  for (var in variable_list)
  {
  
    keep = F
    # print(var)
    
    name = var$covariate_of_interest
    case = var$case %>% as.character()
    control = var$control %>% as.character()
    
    print(sprintf('name: %s, case: %s, control: %s', name, case, control))
    
    if (case=="" & control=="")
    {
      ### Always allow
      print("continuous variable...keeping")
      new_variable_list[[name]] = var
      next
    } 
    
    temp_master_table = 
      master_table %>% 
      filter(!!as.name(name) %in% c(case, control)) %>%
      droplevels()
    
    crosstab = 
      temp_master_table %>%
      select(c(covariate, name)) %>%
      table()
    
    print(crosstab)
    min_samples = min(crosstab)
    print(sprintf("min samples %s", min_samples))
    
    ### Ignore if not in master table
    if (!(name %in% colnames(master_table)))
    {
      print("not in master table...skipping")
      next
    } 
    
    ### Make sure both levels present
    if ((temp_master_table %>% pull(name) %>% unique() %>% length()) < 2)
    {
      print("only one level. skipping...")
      next
    }
    
    print(sprintf('min_samples (all levels): %d', min_samples))
    
    if (min_samples >= num_samples_threshold)
    {
      print("enough samples...keeping")
      new_variable_list[[name]] = var
      next
    }else
    {
      print(sprintf('%s has only %d samples...skipping', name, min_samples))
      next
    }

  }
  
  print("retained variables:")
  print(new_variable_list %>% names())
  
  return(new_variable_list)
}


doMultipleRankRegression = function(
  var,
  predictors,
  master_table,
  rank=T
)
{
  print(sprintf('regression for %s', var))

  formula_rhs = paste0(names(predictors), collapse=' + ')
  if (rank==T)
  {
    formula = paste0('rank(', var, ')', '~', formula_rhs) %>% as.formula()
  } else
  {
    formula = paste0(var, '~', formula_rhs) %>% as.formula()
  }
  # print(formula)

  fit = lm(formula, data=master_table)

  return(fit)
}

getRegressionPvals = function(
  fit,
  contrast_names,
  index_name,
  logistic=F
)
{

  regression_results_table =
    fit %>%
    summary() %>%
    coefficients()
  regression_contrasts = rownames(regression_results_table)

  contrasts = intersect(contrast_names, regression_contrasts)
  
  if (logistic)
  {
    pval_column = 'Pr(>|z|)'
  } else
  {
    pval_column = 'Pr(>|t|)'
  }

  pvals = regression_results_table[contrasts, pval_column] %>% as.list()

  # pvals =
  #   fit %>%
  #   summary() %>%
  #   coefficients() %>%
  #   .[setdiff(contrast_names,rownames(.)), 'Pr(>|t|)'] %>%
  #   as.list() %>%
  #   set_names(c(contrast_names[1], tail(names(.), -1)))

  pvals$Index=index_name

  return(pvals)
}

getRegressionEffectSizes= function(
  fit,
  contrast_names,
  index_name
)
{

  regression_results_table =
    fit %>%
    summary() %>%
    coefficients()
  regression_contrasts = rownames(regression_results_table)

  contrasts = intersect(contrast_names, regression_contrasts)

  effect_sizes = regression_results_table[contrasts, 'Estimate'] %>% as.list()

  # effect_sizes =
  #   fit %>%
  #   summary() %>%
  #   coefficients() %>%
  #   .[contrast_names, 'Estimate'] %>%
  #   as.list() %>%
  #   set_names(c(contrast_names[1], tail(names(.), -1)))

  effect_sizes$Index=index_name

  return(effect_sizes)
}

getRegressionStandardError= function(
  fit,
  contrast_names,
  index_name
)
{

  regression_results_table =
    fit %>%
    summary() %>%
    coefficients()
  regression_contrasts = rownames(regression_results_table)

  contrasts = intersect(contrast_names, regression_contrasts)

  standard_error = regression_results_table[contrasts, 'Std. Error'] %>% as.list()

  # standard_error =
  #   fit %>%1
  #   summary() %>%
  #   coefficients() %>%
  #   .[contrast_names, 'Std. Error'] %>%
  #   as.list() %>%
  #   set_names(c(contrast_names[1], tail(names(.), -1)))

  standard_error$Index=index_name

  return(standard_error)
}


setFactorsLevels = function(
  df,
  variables
  )
{
  for (var in observational_variables)
  {
    name = var$covariate_of_interest
    case = var$case
    control = var$control
    # print(sprintf('name: %s, case: %s, control: %s', name, case, control))

    if (case != '' & control != '')
    {
      # print('ok')
      other_levels = setdiff(df[[name]], c(case, control))
      df = mutate(df, !!name := factor(
        !!as.name(name),
        levels=c(control, case, other=other_levels))
        )
    }

  }

  return(df)
}


filterVarsBySampleThreshold = function(
  master_table,
  threshold,
  variable_list
  )
{
  new_variable_list = list()

  for (var in variable_list)
  {
    name = var$covariate_of_interest

    ### Ignore if not in master table
    if (!(name %in% colnames(master_table)))
    {
      print(sprintf('%s not in master table. skipping...', name))
      # print("next")
      next
    }

    values = master_table %>% pull(!!name)
    counts = table(values)

    if (is.factor(values))
    {
      print(counts)
    }

    min_samples = min(counts)

    print(sprintf('min_samples for %s: %d', name, min_samples))

    if (is.factor(values) & min_samples < threshold)
    {
      print(sprintf('%s has %d < %d samples. skipping...',
                    name,
                    min_samples,
                    threshold
                    ))
      next
    }


    new_variable_list[[name]] = var
  }

  return(new_variable_list)
}


getNumSamplesPerGroup = function(
    master_table,
    variable,
    as_vector=F
  )
{
  name = var$covariate_of_interest
  case = var$case %>% as.character()
  control = var$control %>% as.character()
  print(sprintf('name: %s, case: %s, control: %s', name, case, control))

  num_samples_per_level =
    master_table %>%
    select(name) %>%
    table()

  if (as_vector)
  {
    num_samples_per_level = num_samples_per_level %>% as.vector()
  } else
  {
    num_samples_per_level = num_samples_per_level %>% as.list()
  }

  return(num_samples_per_level)
}


makeContrastNames = function(
  variables,
  master_table
)
{
  contrast_names = list()

  for (var in variables)
  {
    name = var$covariate_of_interest
    values = master_table %>% pull(!!name)

    if (is.factor(values))
    {
      var_levels = levels(values)
      ref_level = var$control
      contrast_levels = var_levels[var_levels != ref_level]

      names_to_add = paste0(name, contrast_levels)
    } else
    {
      names_to_add = name
    }

    print(names_to_add)
    contrast_names[[name]] = names_to_add
  }

  contrast_names =
    contrast_names %>%
    unlist() %>%
    unname()

  return(contrast_names)
}


makeRegressionStatsTemplate = function(variables)
{
  regression_stats_template = data.frame(
    Index=character()
  )

  for (var in variables)
  {
    name = var$covariate_of_interest
    print(name)
    regression_stats_template =
      regression_stats_template %>%
      mutate(!!var$covariate_of_interest := numeric())
  }

  return(regression_stats_template)
}

makeRegressionStatsContrastTemplate = function(
  contrast_names
  )
{
  regression_stats_template = data.frame(
    Index=character()
  )

  for (name in contrast_names)
  {
    regression_stats_template =
      regression_stats_template %>%
      mutate(!!name := numeric())
  }

  return(regression_stats_template)
}



doTwoGroupTests = function(
  exp_vars,
  test_var,
  master_table,
  test_group
)
{
  ### test data
  # exp_vars = c(raw_exp_vars, calculated_exp_vars)
  # test_var = amd_only_variables$CNV_Either_Eye
  # master_table = amd_only_master_table
  # test_group = 'AMD_Only'

  name = test_var$covariate_of_interest
  case = test_var$case
  control = test_var$control
  reference = test_var$labels$reference
  comparison = test_var$labels$comparison

  # print(sprintf('name %s, ref %s, comp %s', name, reference, comparison))

  pval_list = list(
    TestGroup=test_group,
    TestVariable=name,
    Reference=reference,
    Comparison=comparison
  )

  for (var in exp_vars)
  {

    print(sprintf('test_var %s, var %s', name, var))

    index_name = paste0(test_group, '_', var)
    # print(index_name)

    formula = paste0(var, ' ~ ', name) %>% as.formula()

    temp_master_table =
      master_table %>%
      filter(!!as.name(name) %in% c(control, case)) %>%
      droplevels()

    count_table =
      temp_master_table %>%
      select(name) %>%
      table()
    min_samples = min(count_table)
    num_levels = length(count_table)

    # temp_master_table %>% pull(test_var$covariate_of_interest) %>% table() %>% print()

    if (min_samples < 3)
    {
      # pval_list[var] = NA
      print(sprintf('%s has at least one level with %d < %d samples skipping...',
                    name,
                    min_samples,
                    3
            ))
      pval_list[var] = NULL
    } else

    if (num_levels != 2)
    {
      print(sprintf('%s has %d levels, not 2. skipping...',
                    name,
                    num_levels
      ))
      pval_list[var] = NULL
    } else

    {
      fit = wilcox.test(
        formula,
        data=temp_master_table,
        alternative='two.sided'
      )
      # summary(fit)

      pval_list[var] = fit$p.value
    }

  }

  return(pval_list)

}

# exp_vars = c(raw_exp_vars, calculated_exp_vars)
# test_var = amd_only_variables$GA_No_CNV_Either_Eye
# master_table = amd_only_master_table
# test_group = 'AMD_Only'
#
# pval_list = doTwoGroupTests(
#   exp_vars = c(raw_exp_vars, calculated_exp_vars),
#   test_var = test_var,
#   master_table = amd_only_master_table,
#   test_group = 'AMD_Only'
# )
#
# pval_list
#
# for (test_var in amd_only_variables)
# {
#
#   name = test_var$covariate_of_interest
#   print(name)
#
#   pval_list = doTwoGroupTests(
#     exp_vars = c(raw_exp_vars, calculated_exp_vars),
#     test_var = test_var,
#     master_table = amd_only_master_table,
#     test_group = 'AMD_Only'
#   )
#
#   amd_only_two_group_stats %<>%
#     add_row(!!!pval_list)
# }

makeBoxAndDotplot = function(
  master_table,
  indices,
  var_data=NULL,
  aesthetics, ### list(x_var='', color_var='', facet_var='')
  annotation_data=c(),
  include=c("dots", "box"),
  title=NULL,
  xlabel=NULL,
  ylabel=NULL
)
{
  if (!is.null(var_data))
  {
    name = var_data$covariate_of_interest
    case = var_data$case
    control = var_data$control
  } else
  {
    name = ""
    case = ""
    control = ""
  }
  print(sprintf("name %s, case %s, control %s", name, case, control))

  x_var = aesthetics$x_var
  color_var = aesthetics$color_var
  facet_var = aesthetics$facet_var

  print(sprintf("x_var %s, color_var %s, facet_var %s", x_var, color_var, facet_var))
  print(indices)

  cols_to_gather = c(indices, x_var, color_var)
  if (facet_var != '')
  {
    print("got facet_var")
    cols_to_gather = c(cols_to_gather, facet_var)
  }

  ### Because "index" is always present as a grouping column,
  ### if it is going to be used in plot aesthetics, that is it is
  ### one of x_var, color_var, or facet_var, it must be left
  ### out of the gathered columns.

  ### NOTE: 'index' is different from the variable `indices` which
  ### is a list of the alpha diversity indices to plot which are
  ### columns in the input data.
  cols_to_gather = setdiff(cols_to_gather, c('index'))

  print("forming plot data")
  plot_data =
    master_table %>%
    select(!!cols_to_gather) %>%
    # gather(key='index', value='value', -!!cols_to_not_gather) %>%
    gather(key='index', value='value', indices) %>%
    mutate(label=index)

  if (!is.null(var_data))
  {
    plot_data =
      plot_data %>%
      mutate(label=case_when(
        !!as.name(x_var)==case ~ var_data$labels$comparison,
        !!as.name(x_var)==control ~ var_data$labels$reference,
        TRUE ~ index
      ))
  }


  print('colnames plot_data')
  print(colnames(plot_data))

  ### Generate ggplot Object
  plt = ggplot(
    data=plot_data,
    mapping = aes_string(
      x='label',
      y='value',
      color=color_var
    )
  )

  ### Add Beeswarm Layer
  if ("dots" %in% include)
  {
    plt = plt +
      geom_quasirandom(
        width=0.2,
        method='smiley',
        alpha=0.7,
        size=0.8,
        dodge.width=1
      )
  }

  ### Add Box and Whisker Layer
  if ("box" %in% include)
  {
    # geom_boxplot(alpha=0.1, fill='black', colour='black', size=0.5, varwidth=T, width=0.7) +
    plt = plt +
      geom_boxplot(
        aes_string(
          x='label',
          y='value',
          color=color_var
        ),
        alpha=0.1,
        # color='black',
        size=0.5,
        width=0.7
      )
  }

  ### Add Annotations
  if (!is.null(dim(annotation_data)))
  {

    print('filtering annotation data')
    annotations=annotation_data %>%
      filter(
        index %in% indices,
        TestVariable==x_var
      )

    print("adding geom text")

    plt = plt +
      geom_text(
        data=annotations,
        aes(x=xloc, y=yloc, label=pvals, color=index),
        color='black'
      )
  }

  ### Add Facet
  if (facet_var != '')
  {
    print("got facet_var 2")
    plt = plt + facet_wrap(
      as.formula(paste('~',facet_var)),
      scales='free',
      shrink=F
    )
  }

  if (!is.null(var_data))
  {
    legend_labels = sprintf(
      '(%s) %s',
      c(case, control),
      c(var_data$labels$comparison,
        var_data$labels$reference)
    )

    # print(legend_labels)

    plt = plt +
      scale_color_discrete(
        name = name,
        breaks=c(case, control),
        labels=legend_labels
      )
  }

  plt = plt +
    labs(
      title=title,
      x=xlabel,
      y=ylabel
    )


  plt

  return(plt)
}



plotAllTwoGroupTests = function(
  variables,
  master_table,
  indices,
  title_template,
  annotation_data
)
{

  print("plotAllTwoGroupTests")

  for (var in variables)
  {
    name = var$covariate_of_interest
    case = var$case
    control = var$control

    print(sprintf('name: %s, case: %s, control: %s', name, case, control))
    print("labels")
    print(var$labels %>% unlist())

    if (!all(c(case, control) %in% master_table[[name]]))
    {
      print("not all levels present. skipping...")
      next
    }

    title = paste(title_template, name)

    temp_master_table =
      master_table %>%
      filter(!!as.name(name) %in% c(case, control))

    plt = makeBoxAndDotplot(
      master_table=temp_master_table,
      indices=indices,
      var_data=var,
      aesthetics = list(
        x_var=name,
        color_var=name,
        facet_var='index'
      ),
      annotation_data=annotation_data,
      title=title
    )

    print(plt)
  }
}


getPvalAnnotations = function(
  variables,
  pval_height_factor,
  master_table,
  two_group_stats
)
{
  num_annotations = length(variables)

  pval_annotations = data.frame(
    index = variables,
    xloc = rep(1.5, num_annotations),
    yloc = pval_height_factor*master_table %>%
      select(variables) %>%
      drop_na() %>%
      summarize_all(max) %>%
      unlist()
  )

  pval_annotations =
    two_group_stats %>%
    gather(
      key="index",
      value='pvals',
      !!variables
    ) %>%
    left_join(pval_annotations, by='index') %>%
    mutate(pvals = paste0('p = ', round(pvals, 3)))
}

# makeTwoGroupPlot = plotTwoGroupTest
# plotTwoGroupTests = plotAllTwoGroupTests


plotEffectSizes = function(
  pvals, # rows=variables, cols=response_vars, values=pvals
  effect_sizes, # rows=variables, cols=response_vars, values=effect_sizes
  response_var, # Is added to title
  title_template='',
  effect_size_template='',
  category_label=''
)
{

  predictors = pvalues$Predictor

  # tall_pvals = pvalues %>% gather(key='response_var', value='pvalue', -Predictor)
  # tall_effect_sizes = effect_sizes %>% gather(key='response_var', value='effect_size', -Predictor)
  # plot_data = inner_join(tall_pvals, tall_effect_sizes, by=c('Predictor', 'response_var'))

  pvalue_df =
    pvalues %>%
    select(Predictor, response_var) %>%
    rename(pvalue = !!response_var)

  effect_size_df =
    effect_sizes %>%
    select(Predictor, response_var) %>%
    rename(effect_size = !!response_var)

  plot_data = inner_join(pvalue_df, effect_size_df, by='Predictor')

  title = paste(title_template, response_var)

  plt =
    plot_data %>%
    ggplot() +
    geom_bar(
      aes(
        x=Predictor,
        y=log(abs(effect_size))*sign(effect_size),
        fill=pvalue
      ),
      stat='identity'
    ) +
    coord_flip() +
    ggtitle(title) +
    ylab(paste(effect_size_template, "as Effect Size")) +
    xlab(category_label) +
    theme(
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      plot.title = element_text(size=12)
    )

  print(plt)
}


getMultipleRegressionStats = function(
  predictor_vars,
  response_vars,
  master_table,
  stats=c('pvalues', 'effect_sizes', 'std_errors'),
  response_var_name="",
  adjustment_method="",
  parametric=F
)
{

  # predictor_vars=all_variables
  # response_vars=calculated_exp_vars
  # master_table=all_master_table
  # stats=c('pvalue', 'effect_size', 'std_error')
  # response_var_name="Index"
  # adjustment_method="BH"
  # parametric=T

  ### Get contrast names
  contrast_names = makeContrastNames(
    predictor_vars,
    master_table
  ) %>%
    unlist() %>%
    unname()

  ### Create stats template
  regression_stats = list()

  stats_for_contrast_template = data.frame(
    response_var_name=character(),
    pvalue=numeric(),
    # padj=numeric(),
    effect_size=numeric(),
    std_error=numeric()
  )

  for (contrast in contrast_names)
  {
    regression_stats[[contrast]] = stats_for_contrast_template
  }

  ### Perform regression in loop over response vars
  for (var in response_vars)
  {
    # print(sprintf('response var: %s', var))

    ### Build Formula
    if (parametric)
    {
      lhs = var
    } else
    {
      lhs = sprintf('rank(%s)', var)
    }

    predictor_names = predictor_vars %>% names() %>% unname()
    rhs = paste(predictor_names, collapse=' + ')
    formula_string = paste0(lhs, ' ~ ', rhs)
    formula = as.formula(formula_string)

    # print(sprintf('formula: %s', formula_string))

    ## Peroform Regression
    # print("peform regression")
    fit = lm(formula=formula, data=master_table)

    ### Collect pvalues
    if ('pvalues' %in% stats)
    {
      pvalues = getRegressionPvals(
        fit,
        contrast_names,
        index_name=NULL
      )
      pvalues['response_var_name'] = var
      # pvalues$Subgroup = subgroup

      # regression_pvalues = regression_pvalues %>% add_row(!!!pvalues)

    }

    ### Collect effect sizes
    if ('effect_sizes' %in% stats)
    {

      effect_sizes = getRegressionEffectSizes(
        fit,
        contrast_names,
        index_name=NULL
      )
      effect_sizes['response_var_name'] = var
      # effect_sizes$Subgroup = subgroup

      # regression_effect_sizes = regression_effect_sizes %>% add_row(!!!effect_sizes)
    }

    ### Collect Standard Errors
    if ('std_errors' %in% stats)
    {
      std_errors = getRegressionStandardError(
        fit,
        contrast_names,
        index_name=NULL
      )
      std_errors['response_var_name'] = var
      # std_errors$Subgroup = subgroup

      # regression_std_errors = regression_std_errors %>% add_row(!!!std_errors)
    }


    for (contrast in contrast_names)
    {
      # print(var)
      # print(contrast)
      new_row = list()
      new_row$response_var_name = var

      if ("pvalues" %in% stats)
      {
        new_row$pvalue = pvalues[[contrast]]
      }
      if ("effect_sizes" %in% stats)
      {
        new_row$effect_size = effect_sizes[[contrast]]
      }
      if ("std_errors" %in% stats)
      {
        new_row$std_error = std_errors[[contrast]]
      }
      # print(new_row)
      # print(regression_stats[[contrast]] %>% colnames)
      regression_stats[[contrast]] %<>% add_row(!!!new_row)
    }

  }

  ### Redefine response var column name
  for (contrast in contrast_names)
  {
    regression_stats[[contrast]] %<>%
      rename(!!response_var_name := response_var_name) %>%
      arrange(pvalue)
  }

  ### Multiple comparison adjustment of pvalues
  if (adjustment_method != "")
  {
    for (contrast in contrast_names)
    {
      regression_stats[[contrast]][['padj']] = p.adjust(
        regression_stats[[contrast]][['pvalue']],
        method=adjustment_method
      )
    }
  }

  return(regression_stats)

}



getLogisticRegressionStats = function(
    base_predictor_vars,
    additional_predictors,
    logistic_response_var,
    master_table,
    stats=c('pvalue', 'effect_size', 'std_error'),
    response_var_name="",
    adjustment_method=""
  )
{
  
  
  # base_predictor_vars=observational_variables[c('Age', 'Gender', 'Tissue_code')]
  # additional_predictors=taxa
  # logistic_response_var='CaseString'
  # master_table=all_master_table
  # stats=c('pvalue', 'effect_size', 'std_error')
  # response_var_name='CaseString'
  # index_name=NULL
  # adjustment_method='BH'

  
  ### Get contrast names
  base_contrast_names = makeContrastNames(
    base_predictor_vars,
    master_table
  ) %>%
    unlist() %>%
    unname()
  
  ### Create stats template
  regression_stats = list()
  
  stats_for_contrast_template = data.frame(
    contrast=character(),
    pvalue=numeric(),
    # padj=numeric(),
    effect_size=numeric(),
    std_error=numeric()
  )
  
  for (var in additional_predictors)
  {
    regression_stats[[var]] = stats_for_contrast_template
  }
  
  
  ### Perform regression in loop over response vars
  for (var in additional_predictors)
  {
    # print(sprintf('response var: %s', var))
    
    ### Build Formula
    lhs = logistic_response_var
    
    predictor_names = base_predictor_vars %>% names() %>% unname()
    all_predictor_names = c(var, predictor_names)
    all_contrast_names = c(base_contrast_names, var)
    
    rhs = paste(all_predictor_names, collapse=' + ')
    formula_string = paste0(lhs, ' ~ ', rhs)
    formula = as.formula(formula_string)
    
    # print(sprintf('formula: %s', formula_string))
    
    ## Peroform Regression
    # print("peform regression")
    fit = glm(
      formula=formula, 
      data=master_table,
      family=binomial(link='logit')
      )
    
    ### Collect pvalues
    if ('pvalue' %in% stats)
    {
      pvalues = getRegressionPvals(
        fit,
        contrast_names = all_contrast_names,
        index_name=NULL,
        logistic=T
      )
      
      # pvalues[['self']] = pvalues[[var]]
      # pvalues[[var]] = NULL
      # pvalues['response_var_name'] = logistic_response_var
      # pvalues$Subgroup = subgroup
      
      # regression_pvalues = regression_pvalues %>% add_row(!!!pvalues)
      
    }
    
    ### Collect effect sizes
    if ('effect_size' %in% stats)
    {
      
      effect_sizes = getRegressionEffectSizes(
        fit,
        contrast_names = all_contrast_names,
        index_name=NULL
      )
      effect_sizes['response_var_name'] = var
      # effect_sizes$Subgroup = subgroup
      
      # regression_effect_sizes = regression_effect_sizes %>% add_row(!!!effect_sizes)
    }
    
    ### Collect Standard Errors
    if ('std_error' %in% stats)
    {
      std_errors = getRegressionStandardError(
        fit,
        contrast_names = all_contrast_names,
        index_name=NULL
      )
      std_errors['response_var_name'] = var
      # std_errors$Subgroup = subgroup
      
      # regression_std_errors = regression_std_errors %>% add_row(!!!std_errors)
    }
    
    
    stats_df = stats_for_contrast_template
    
    for (contrast in all_contrast_names)
    {
      # print(var)
      # print(contrast)
      new_row = list()
      new_row$contrast = contrast

      if ("pvalue" %in% stats)
      {
        new_row$pvalue = pvalues[[contrast]]
      }
      if ("effect_size" %in% stats)
      {
        new_row$effect_size = effect_sizes[[contrast]]
      }
      if ("std_error" %in% stats)
      {
        new_row$std_error = std_errors[[contrast]]
      }
      # print(new_row)
      # print(regression_stats[[contrast]] %>% colnames)
      stats_df %<>% add_row(!!!new_row)
    }
    
    regression_stats[[var]] = stats_df
    
  }
  
  ### Redefine response var column name
  # for (contrast in contrast_names)
  # {
  #   regression_stats[[contrast]] %<>%
  #     rename(!!response_var_name := response_var_name) %>%
  #     arrange(pvalue)
  # }
  
  ### Multiple comparison adjustment of pvalues
  # if (adjustment_method != "")
  # {
  #   
  #   pvalues = lapply(regression_stats, function(x) x$pvalue)
  #   
  #   for (contrast in contrast_names)
  #   {
  #     regression_stats[[contrast]][['padj']] = p.adjust(
  #       regression_stats[[contrast]][['pvalue']],
  #       method=adjustment_method
  #     )
  #   }
  # }
  
  return(regression_stats)
  
}


# predictor_vars=amd_only_variables
# response_vars=top_n_features
# master_table=iga_index_master_table
# stats=c('pvalues', 'effect_sizes', 'std_errors')
# response_var_name="Taxa"
# adjustment_method="BH"
# parametric=F
#
# regression_stats = getMultipleRegressionStats(
#   predictor_vars=amd_only_variables,
#   response_vars=top_n_features,
#   master_table=iga_index_master_table,
#   stats=c('pvalues', 'effect_sizes', 'std_errors'),
#   response_var_name="Taxa",
#   adjustment_method="BH",
#   parametric=F
# )

# regression_stats = getMultipleRegressionStats(
#   predictor_vars=amd_only_variables,
#   response_vars=top_n_features,
#   master_table=iga_index_master_table,
#   stats=c('pvals', 'effect_sizes', 'std_errors'),
#   response_var_name="Taxa",
#   adjustment_method="BH"
# )




# predictors = c('x1', 'x2', 'x3')
# response_vars = c('A', 'B', 'C')
#
# pvalues =
#   matrix(runif(9), nrow=3, ncol=3) %>%
#   set_rownames(predictors) %>%
#   set_colnames(response_vars) %>%
#   data.frame() %>%
#   rownames_to_column('Predictor')
#
# effect_sizes =
#   matrix(rnorm(9), nrow=3, ncol=3) %>%
#   set_rownames(predictors) %>%
#   set_colnames(response_vars) %>%
#   data.frame() %>%
#   rownames_to_column('Predictor')
#
# plotEffectSizes(
#   pvals = pvalues,
#   effect_sizes = effect_sizes,
#   response_var = 'C',
#   title_template='Response Variable:',
#   effect_size_template='Regression Coefficient',
#   category_label='Predictor'
# )


# a = pvalues %>% gather(key='response_var', value='pvalue', -Predictor)
# b = effect_sizes %>% gather(key='response_var', value='effect_size', -Predictor)
# c = inner_join(a,b, by=c('Predictor', 'response_var'))

