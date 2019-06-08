library(dplyr)
library(tidyr)
library(magrittr)
library(DESeq2)
library(ggplot2)
library(ggbeeswarm)


getIgAIndices = function(
    ### Sample-wise normalized counts
    abundances_table, 
    sample_data, 
    ### Column of sample_data that contains iga status
    iga_colname='IGA', 
    ### Values that iga status column has
    iga_fraction_names=list(pos='Pos', neg='Neg', allbac='AllBac'),
    ### Value to inject into relative abundance to avoid log(0) or /0 errors
    zero_correction=0,
    ### Columns to add to the results, such as taxa and p-values
    extra_cols=c()
  )
{
  
  print("getting iga- samples")
  iga_neg_samples = 
    sample_data %>%
    filter(!!as.name(iga_colname) == iga_fraction_names$neg) %>%
    pull(SampleName)
  print(iga_neg_samples)
  
  print("getting iga+ samples")
  iga_pos_samples = 
    sample_data %>%
    filter(!!as.name(iga_colname) == iga_fraction_names$pos) %>%
    pull(SampleName)
  print(iga_pos_samples)
  
  print("getting + and - abundances")
  iga_neg = 
    abundances_table %>% 
    select(iga_neg_samples)
  ### Change colnames from SampleName to SubjectID
  colnames(iga_neg) = 
    sample_data %>%
    filter(SampleName %in% iga_neg_samples) %>%
    pull(SubjectID)
    
    # gsub("([A-Z0-9]+)_.*$", "\\1", colnames(iga_neg))

  iga_pos = 
    abundances_table %>% 
    select(iga_pos_samples)
  # colnames(iga_pos) = gsub("([A-Z0-9]+)_.*$", "\\1", colnames(iga_pos))
  ### Change colnames from SampleName to SubjectID
  colnames(iga_pos) = 
    sample_data %>%
    filter(SampleName %in% iga_pos_samples) %>%
    pull(SubjectID)
  
  print("getting corrected logs")
  log_neg = log(iga_neg + zero_correction)
  log_pos = log(iga_pos + zero_correction)
  
  print("calculating index")
  iga_indices = (log_neg - log_pos)/(log_neg + log_pos)
  
  if (length(extra_cols) != 0)
  {
    iga_indices = 
      iga_indices %>%
      cbind(abundances_table %>% select(extra_cols))
  }
  
  return(iga_indices)
}


getRelAbund = function(
  counts,
  use_cols=c()
)
{
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


getICIScores = function(
  iga_neg,
  iga_pos,
  count_type="raw",
  zero_correction=0,
  extra_cols=c()
)
{
  if (count_type == "raw")
  {
    neg = iga_neg + zero_correction
    pos = iga_pos + zero_correction
  } else if(count_type == "rel")
  {
    neg = iga_neg + zero_correction
    pos = iga_pos + zero_correction
  } else
  {
    warning("Invalid count type. Use \"raw\" or \"rel\"")
    return()
  }
  
  iga_scores = pos/neg
  
  if (length(extra_cols) != 0)
  {
    iga_scores = 
      iga_scores %>%
      cbind(extra_cols)
  }
  
  return(iga_scores)
}


removeOutlierRows = function(
  df,
  min_pct,
  max_pct,
  use_cols=c()
)
{
  
  print(df)
  
  if ( length(use_cols) == 0 )
  {
    use_cols = colnames(df)
  }
  
  print(use_cols)
  
  print(df %>% select(!!use_cols))
  
  quantile_probs = c(min_pct, max_pct)
  print(quantile_probs)
  
  cutoff_values = 
    df %>% select(use_cols) %>%
    unlist() %>%
    quantile(c(min_pct, max_pct), na.rm=T) %>%
    unlist()
  
  print(cutoff_values)
  
  min_cutoff = cutoff_values[1]
  max_cutoff = cutoff_values[2]
  print(sprintf("max cutoff: %0.2f, min cutoff: %0.2f", max_cutoff, min_cutoff))
  
  df = 
    df %>%
    mutate(
      mins=apply(.[use_cols], 1, min, na.rm=T),
      maxs=apply(.[use_cols], 1, max, na.rm=T)
    ) %>%
    filter(
      mins>=min_cutoff, 
      maxs<=max_cutoff
    ) %>%
    select(-mins, -maxs)
  
  return(df)
}

getTopNbyRowMean = function(
  data_table,
  use_samples=c(),
  num_top_features
)
{
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

makeIndexBarPlot = function(
  data_with_pvals,
  sample_metadata,
  group_data, ### = list(group_name='GroupName', classA='Green', classB='Red')
  index_title_name,
  pval_colname,
  pval_name='Unadjusted'
  )
{

  
  # data_with_pvals=iga_index_with_pvals
  # sample_metadata = sample_metadata
  # group_data=list(group_name='AREDS', classA='N', classB='Y')
  # index_title_name='IgA Index'
  # pval_colname='pvals'
  # pval_name='Undjusted P-Values'

  # print("the data")
  # print(class(data_with_pvals))
  # print(typeof(data_with_pvals))
  # print(data_with_pvals$pvals)
  # print(dim(data_with_pvals))
  # head(data_with_pvals)
  # print('makeIndexBarPlot')
  # print(group_data)
  # print("data_with_pvalsw colnames")
  # print(data_with_pvals %>% colnames())
  # print(data_with_pvals)
  # data_with_pvals %>% print()


  group_name = group_data$group_name
  classA = group_data$classA
  classB = group_data$classB

  # print(sprintf('group_name=%s, classA=%s, classB=%s',
  #   group_name,
  #   classA,
  #   classB
  #   ))

  class_a_subjects =
    sample_metadata %>%
    filter(!!as.name(group_name)==classA) %>%
    pull(SubjectID) %>%
    as.character() %>%
    unique()

  # print(class_a_subjects)

  class_b_subjects =
    sample_metadata %>%
    filter(!!as.name(group_name)==classB) %>%
    pull(SubjectID) %>%
    as.character() %>%
    unique()

  # print(class_b_subjects)

  # print('getting barplot data')
  barplot_data =
    data_with_pvals %>%
    ### Keep only data columns and short glommed taxa names
    select(
      short_glommed_taxa,
      class_a_subjects,
      class_b_subjects,
      pvals=!!pval_colname
      ) %>%
    ### Trasform to long format keeping some columns (specified by - sign)
    ### Long columns are subject ID and iga index value
    gather(
      key='subject',
      value='score',
      -short_glommed_taxa,
      -pvals
    ) %>%
    ### rename pvals (shorter)
    # mutate(pvals = !!pval_colname) %>%
    mutate(class = ifelse(
      subject %in% class_a_subjects,
      classA, classB
      )) %>%
    select(-subject)

  # print("barplot_data")
  # print(barplot_data)

  # print('barplot data colnames')
  # print(barplot_data %>% colnames())

  plt=
    barplot_data %>%
    ggplot() +
    geom_bar(
      aes(
        x=short_glommed_taxa,
        y=score,
        fill=pvals
      ),
      stat='summary',
      fun.y='mean'
    ) +
    facet_grid(cols=vars(class)) +
    coord_flip() +
    labs(
      x='Taxa',
      y=sprintf('Mean %s (across taxa)', index_title_name)
    ) +
    ggtitle(sprintf("%s vs. %s Significance of Taxa by %s",
      classA,
      classB,
      index_title_name
      )) +
    theme(
      plot.title = element_text(size=16, hjust=0.6),
      legend.title = element_text(size=10),
      axis.title = element_text(size=14)
    ) +
    scale_fill_gradient2(low='#000000', mid='#222222', high='#FFFFFF')
  print(plt)
  
}

getWilcoxonPvalsForTaxa = function(
  data, 
  taxa, 
  formula, 
  ... ### extra parameters to pass on to wilcox test
)
{
  rhs = as.character(formula)[[2]]
  pvals = lapply(taxa, function(taxon)
  {
    formula=as.formula(paste(taxon, '~', rhs))
    # print(formula)
    stat = wilcox.test(
      formula=formula,
      data=data, 
      ... 
    )
    
    return(stat$p.value)
  })
}

getStudentsPvalsForTaxa = function(
  data, 
  taxa, 
  formula, 
  ... ### extra parameters to pass on to wilcox test
)
{
  rhs = as.character(formula)[[2]]
  pvals = lapply(taxa, function(taxon)
  {
    formula=as.formula(paste(taxon, '~', rhs))
    # print(formula)
    stat = t.test(
      formula=formula,
      data=data, 
      ... 
    )
    
    return(stat$p.value)
  })
}

makePValPlot1 = function(
  taxa_data, 
  pval_col, 
  data_name,
  group_data,
  subjects
)
{
  p_value = taxa_data %>% pull(pval_col)
  max_value = taxa_data %>% select(subjects) %>% max()
  
  group_name = group_data$group_name
  classA = group_data$classA
  classB = group_data$classB
  
  plt = 
    taxa_data %>%
    select(-pval_col) %>%
    gather(key='subject', value=data_name, -short_glommed_taxa) %>%
    mutate(group_name
           := ifelse(subject %in% case_subjects, 'Case', 'Control')) %>%
    select(-subject) %>%
    ggplot() + 
    geom_quasirandom(
      aes(x=Case, y=data_name, shape=),
      nbins=10, 
      # bandwidth=0.1, 
      width=0.1,
      method='smiley', 
      varwidth = T,
      size=3
    ) + 
    geom_boxplot(
      aes(x=Case, y=data_name),
      alpha=0.2, 
      width=0.3, 
      fill='grey', 
      show.legend = F
    ) +
    annotate(
      "text", 
      x = 1.5, 
      y = max_value*1.2, 
      label = sprintf('p-value = %0.2f', p_value)
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(data_name)
    ) +
    ggtitle(taxa_data$short_glommed_taxa)
  
  print(plt)
  
}


getDataWithPvals = function(
    data,
    metadata,
    taxa_list,
    data_key_column,
    metadata_key_column,
    formula
    )
{
  data = iga_index %>% select(short_glommed_taxa, count_filtered_subjects)
  metadata = sample_metadata
  taxa_list = taxa_list
  data_key_column = 'short_glommed_taxa'
  metadata_key_column = 'SubjectID'
  formula = ~AREDS
  
  rhs = 
    ### Get the elements of the RHS of the formula
    ### as a list
    terms(formula) %>%
    attr(., 'term.labels')
  
  ### Get just needed parts of metadata which are the key column
  ### And any columns used in the formula
  metadata = 
    metadata %>%
    ### Select only needed columns
    select(metadata_key_column, rhs) %>% 
    ### In some cases the key column might not be shared by 
    ### multiple rows. Then need to get distinct rows.
    distinct()
  
  data_with_metadata = getDataCombinedWithMetadata(
    data=data,
    metadata=metadata,
    data_pivot_column=data_key_column,
    metadata_pivot_column=metadata_key_column
    ) %>%
    mutate_at(vars(top_n_features), list(~as.numeric))
  
  # print(head(data_with_metadata))
  
  pvals = getWilcoxonPvalsForTaxa(
    data=data_with_metadata,
    taxa=taxa_list,
    formula=formula
  )
  
  # print(pvals)
  
  padj = p.adjust(pvals, method='fdr')
  
  data_with_pvals = 
    ### Turn the pval lists (by taxa) into a dataframe. It automatically
    ### has the taxa as rownames.
    as.data.frame(
      cbind(
        padj %>% unlist(),
        pvals %>% unlist()
      ),
      stringsAsFactors = F
    ) %>%
    ### Set the colnames (probably not necessary here)
    set_names(c('padj', 'pvals')) %>% 
    ### Move the taxa names to a column for the join
    tibble::rownames_to_column('short_glommed_taxa') %>%
    inner_join(data, by='short_glommed_taxa') %>%
    arrange(padj)

  return(data_with_pvals)
  
}


getDataCombinedWithMetadata = function(
  data,
  metadata,
  data_pivot_column,
  metadata_pivot_column
)
{
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

makePValPlot = function(
  data_with_pvals,
  sample_metadata,
  pval_colname,
  data_name,
  variable_colname,
  variable_name, 
  group_data
)
{
  # 
  # data_with_pvals=iga_index_with_pvals
  # sample_metadata=sample_metadata
  # pval_colname='pvals'
  # data_name='IgA Index'
  # variable_colname='short_glommed_taxa'
  # variable_name="Firmicutes_Subdoligranulum" 
  # group_data=list(group_name='AREDS', classA='N', classB='Y')
  
  data_row = 
    data_with_pvals %>%
    filter(!!as.name(variable_colname) == variable_name)
    
  p_value = 
    data_row %>% 
    pull(pval_colname)
  
  
  group_name = group_data$group_name
  classA = group_data$classA
  classB = group_data$classB
  
  # print(sprintf('group_name=%s, classA=%s, classB=%s',
  #   group_name,
  #   classA,
  #   classB
  #   ))
  
  class_a_subjects =
    sample_metadata %>%
    filter(!!as.name(group_name)==classA) %>%
    pull(SubjectID) %>%
    as.character() %>%
    unique()
  
  # print(class_a_subjects)
  
  class_b_subjects =
    sample_metadata %>%
    filter(!!as.name(group_name)==classB) %>%
    pull(SubjectID) %>%
    as.character() %>%
    unique()
  
  plot_data = 
    data_row %>%
    select(-pval_colname) %>%
    gather(key='subject', value=!!data_name, -short_glommed_taxa) %>%
    mutate(!!group_name := ifelse(subject %in% class_a_subjects, classA, classB)) %>%
    select(-subject)
  
  max_value = max(plot_data[[data_name]])
  
  plt = 
    ggplot(plot_data) + 
    geom_quasirandom(
      aes(
        x=!!as.name(group_name), 
        y=!!as.name(data_name), 
        shape=!!as.name(group_name)
        ),
      nbins=10,
      # bandwidth=0.1,
      width=0.1,
      method='smiley',
      varwidth = T,
      size=3
    ) +
    geom_boxplot(
      aes(x=!!as.name(group_name), y=!!as.name(data_name)),
      alpha=0.2, 
      width=0.3, 
      fill='grey', 
      show.legend = F
    ) +
    annotate(
      "text", 
      x = 1.5, 
      y = max_value*1.2, 
      label = sprintf('p-value = %0.2f', p_value)
    ) +
    theme(
      axis.title.x = element_text(variable_name),
      axis.title.y = element_text(data_name)
    ) +
    ggtitle(variable_name)
  
  print(plt)
  
}
