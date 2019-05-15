library(dplyr)
library(DESeq2)
library(ggplot2)
library(tidyr)

# getIgAIndeces = function(
#     iga_neg,
#     iga_pos,
#     count_type="raw",
#     zero_correction=0,
#     extra_cols=c()
#   )
# {
#   if (count_type == "raw")
#   {
#     log_neg = log(iga_neg + zero_correction)
#     log_pos = log(iga_pos + zero_correction)
#   } else if(count_type == "rel")
#   {
#     log_neg = log(iga_neg + zero_correction)
#     log_pos = log(iga_pos + zero_correction)
#   } else
#   {
#     warning("Invalid count type. Use \"raw\" or \"rel\"")
#     return()
#   }
#   
#   iga_indices = (log_neg - log_pos)/(log_neg + log_pos)
#   
#   if (length(extra_cols) != 0)
#   {
#     iga_indices = 
#       iga_indices %>%
#       cbind(extra_cols)
#   }
#   
#   return(iga_indices)
# }


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
    mutate_if(is.numeric, funs(./sum(.)))
  
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
  N
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