library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggbeeswarm)
library(pwr)


removeOutlierRows = function(
  df,
  min_pct,
  max_pct,
  use_cols=c()
)
{

  ### Experimental

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


getWilcoxonPvalsForTaxa = function(
  data,
  taxa,
  formula,
  ... ### extra parameters to pass on to wilcox test
)
{

  ### Developer note: add parameter for test: wilcox.test, t.test,
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

getDataWithPvals = function(
    data,
    metadata,
    taxa_list,
    data_key_column,
    metadata_key_column,
    formula,
    test=wilcox.test
    )
{
  # data = iga_index %>% select(short_glommed_taxa, count_filtered_subjects)
  # metadata = sample_metadata
  # taxa_list = taxa_list
  # data_key_column = 'short_glommed_taxa'
  # metadata_key_column = 'SubjectID'
  # variable_data = list(
  #   covariate_of_interest='CaseString',
  #   case='AMD',
  #   control='Control'
  # )
  # variable_data = variables_of_interest[['ARMS2.rs10490924']]
  # formula = as.formula(paste('~', variable_data$covariate_of_interest))

  variable_name = variable_data$covariate_of_interest
  variable_levels = c(variable_data$case, variable_data$control)

  rhs =
    ### Get the elements of the RHS of the formula
    ### as a list
    terms(formula) %>%
    attr(., 'term.labels')

  ### Get just needed parts of metadata which are the key column
  ### And any columns used in the formula
  filtered_metadata =
    metadata %>%
    ### Select only needed columns
    select(metadata_key_column, rhs) %>%
    ### In some cases the key column might not be shared by
    ### multiple rows. Then need to get distinct rows.
    distinct() %>%
    ### Filter out any values for the variable except the two levels
    ### we are interested in. Can only use two for this test.
    filter(!!as.name(variable_name) %in% variable_levels)

  data_with_metadata = getDataCombinedWithMetadata(
    data=data,
    metadata=filtered_metadata,
    data_pivot_column=data_key_column,
    metadata_pivot_column=metadata_key_column
    )
  # %>%
  #   mutate_at(vars(top_n_features), list(~as.numeric))

  # print(head(data_with_metadata))

  # print(formula)

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


makePvalPlot = function(
    data_with_metadata, ### rows=subjects/samples, cols=taxa + metadata
    data_name="", ### for title only
    data_col, ### e.g. taxon
    variable_data, ### list(covariate_of_interest="A", case='Y', control='N')
    p_value, ### list(type=type, value=value)
    labels=list(case='Case', control='Control') ### etc.
  )
{

  ### Developer Note:
  ### variable_data and labels seem redundant. Labels has the titles or names such as
  ### Risk/"Non-Risk" that may be different than case/control.

  ### Data for testing
  # data_with_metadata=ici_score_master_table
  # data_name='ICI Score'
  # data_col=data_for_pval_plots$short_glommed_taxa[[1]]
  # # grouping_col=list(name='CNV_Either_Eye', values=c(0,1))
  # # grouping_col=list(name='CFH_rs1061170', values=c('TT', 'CC'))
  # row = data_for_pval_plots %>% filter(short_glommed_taxa == rowname)
  #
  # p_value=list(type='Unadjusted', value=data_for_pval_plots %>% pull(Pval) %>% .[[1]])
  # variable_data = observational_variables$CaseString
  # # variable_data = list(
  # # #   covariate_of_interest='ARMS2_rs10490924',
  # # #   control='GG',
  # # #   case='TT'
  # # # )

  variable_name=variable_data$covariate_of_interest
  case = variable_data$case
  control = variable_data$control

  plot_data =
    data_with_metadata %>%
    ### Make sure we only filter out only two classes. Perhaps expand later.
    filter(!!as.name(variable_name) %in% c(case, control)) %>%
    ### Only need the variable name and counts
    select(data_col, variable_name) %>%
    ### Turning it into a factor helps ggplot
    mutate(!!variable_name := factor(
      !!as.name(variable_name),
      # levels=c(paste(control, '(Control)'), paste(case, '(Case)'))))
      levels=c(control,case)
    ))

  stats =
    plot_data %>%
    group_by(!!as.name(variable_name)) %>%
    summarize(N=n(), mu=mean(!!as.name(data_col)))

  SD = sd(plot_data[[data_col]])

  effect_size = abs(diff(stats$mu))/SD

  print(sprintf("taxon=%s, variable=%s", data_col, variable_name))

  if (stats$N[1]>=2 & stats$N[2]>=2)
  {
    power = pwr.t2n.test(
      n1=stats$N[1],
      n2=stats$N[2],
      d=effect_size,
      sig.level=0.2
    )
    print(power)
  } else
  {
    warning("Insufficient sample size for power calculation.")
  }

  num_case =
    plot_data %>%
    filter(!!as.name(variable_name) == case) %>%
    summarise(N=n()) %>%
    pull(N)

  num_control =
    plot_data %>%
    filter(!!as.name(variable_name) == control) %>%
    summarise(N=n()) %>%
    pull(N)



  # print(sprintf('variable_name=%s', variable_name))

  labels = variable_data$labels
  ### Allow for labels other than Case/Control
  if (length(labels) != 0)
  {
    case_label = paste0(case, ' (', labels$reference, ')')
    control_label = paste0(control, ' (', labels$comparison, ')')
    # print(sprintf('control_label=%s, case_label=%s', control_label, case_label))

    plot_data =
      plot_data %>%
      mutate(!!variable_name := ifelse(
        !!as.name(variable_name) == case, case_label, control_label))
  } else
  {
    case_label = case
    control_label = control
  }

  # print(sprintf('control_label=%s, case_label=%s', control_label, case_label))

  # print(plot_data)

  ### Used to position pval annotation
  max_value = max(plot_data[[data_col]])

  plt =
    ggplot(plot_data) +
    ### beeswarm part of plot. maybe aes_string would be easier?
    geom_quasirandom(
      aes(
        x=!!as.name(variable_name),
        y=!!as.name(data_col),
        shape=!!as.name(variable_name)
      ),
      ### Arbitrary choice. Perhaps add as parameter or make functino of count
      nbins=10,
      # bandwidth=0.1,
      ### Is this redundant with nbins?
      width=0.1,
      # method='smiley',
      dodge.width=1,
      varwidth = T,
      size=3
    ) +
    ### Boxplot part. Split by values of variable_name
    geom_boxplot(
      aes(x=!!as.name(variable_name), y=!!as.name(data_col)),
      alpha=0.2,
      width=0.3,
      fill='grey',
      show.legend = F,
      outlier.alpha=1,
      outlier.color='red',
      outlier.fill='red',
      outlier.shape=18,
      outlier.size=3
    ) +
    ### add in pvalue annotation.
    annotate(
      "text",
      x = 1.5,
      y = max_value*1.2,
      label = sprintf('%s p-value = %0.2f', p_value$type, p_value$value)
    ) +
    theme(
      axis.title.x = element_text(variable_name),
      axis.title.y = element_text(data_col)
    ) +
    ggtitle(paste0(data_name, ': ', data_col))
    #+
    # scale_fill_manual(
    #   values=!!as.name(variable_name),
    #   labels = c(paste(case, '(Case)'), paste(control, '(Control)'))
    #   )

  print(plt)
}


makeIndexBarPlot = function(
    data_with_pvals, ### rows are taxa, cols are subjects and pvalues
    sample_metadata,
    variable_data, ### list(covariate_of_interest="A", case='Y', control='N')
    data_name, ### for plot title only
    pval_colname, ### e.g. pvals, padj, whatever
    pval_name='Unadjusted' ### For legend only
  )
{

  ### Data for testing
  # data_with_pvals=iga_index_with_pvals
  # sample_metadata = sample_metadata
  # variable_data=list(variable_name='AREDS', case='N', control='Y')
  # data_name='IgA Index'
  # pval_colname='pvals'
  # pval_name='Undjusted P-Values'
  # variable_data=list(covariate_of_interest='ARMS2_rs10490924', case='GG', control='TT')

  variable_name = variable_data$covariate_of_interest
  case = variable_data$case
  control = variable_data$control

  # print(sprintf('variable_name=%s, case=%s, control=%s',
  #   variable_name,
  #   case,
  #   control
  #   ))

  ### Since subjects are col names, make lists of them to
  ### help selecting easier.
  case_subjects =
    sample_metadata %>%
    filter(!!as.name(variable_name)==case) %>%
    pull(SubjectID) %>%
    as.character() %>%
    unique()

  # print(case_subjects)

  control_subjects =
    sample_metadata %>%
    filter(!!as.name(variable_name)==control) %>%
    pull(SubjectID) %>%
    as.character() %>%
    unique()

  # print(control_subjects)

  # print('getting barplot data')
  barplot_data =
    data_with_pvals %>%
    ### Keep only data columns and short glommed taxa names
    select(
      short_glommed_taxa,
      case_subjects,
      control_subjects,
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
      subject %in% case_subjects,
      paste(case, '(Case)'),
      paste(control, '(Control)')
    )) %>%
    select(-subject)

  # print("barplot_data")
  # print(barplot_data)

  # print('barplot data colnames')
  # print(barplot_data %>% colnames())

  plt=
    barplot_data %>%
    ggplot() +
    ### Barplot fill intensity by pvalues (whichever were chosen)
    geom_bar(
      aes(
        x=short_glommed_taxa,
        y=score,
        fill=pvals
      ),
      stat='summary',
      fun.y='mean'
    ) +
    ### Facet by Case/Control, Yes/No, 0/1, etc.
    facet_grid(cols=vars(class)) +
    ### Put bars sideways
    coord_flip() +
    labs(
      x='Taxa',
      y=sprintf('Mean %s (across taxa)', data_name)
    ) +
    ggtitle(sprintf(
      "%s: %s and Significance of Taxa",
      variable_name,
      data_name
    )) +
    theme(
      plot.title = element_text(size=16, hjust=0.6),
      legend.title = element_text(size=10),
      axis.title = element_text(size=14)
    ) +
    ### Use grayscale that makes only fairly low pvalues dark
  ### Color would be nice, but apparently journals charge for it?!?!?
    scale_fill_gradient2(low='#000000', mid='#222222', high='#FFFFFF')
  print(plt)

}
