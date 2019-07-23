library(tidyverse)
library(magrittr)
library(vegan)
library(ggplot2)
library(ggbeeswarm)

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
  # data_with_metadata=iga_index_and_sample_metadata
  # data_name='IgA Index'
  # data_col=data_for_pval_plots$short_glommed_taxa[[1]]
  # grouping_col=list(name='CNV_Either_Eye', values=c(0,1))
  # grouping_col=list(name='CFH_rs1061170', values=c('TT', 'CC'))
  # p_value=list(type='Unadjusted', value=data_for_pval_plots %>% pull(pvals) %>% .[[1]])
  # variable_data = list(
  #   covariate_of_interest='ARMS2_rs10490924',
  #   control='GG',
  #   case='TT'
  # )
  
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
  
  # stats =
  #   plot_data %>%
  #   group_by(!!as.name(variable_name)) %>%
  #   summarize(N=n(), mu=mean(!!as.name(data_col)))
  # 
  # SD = sd(plot_data[[data_col]])
  # 
  # effect_size = abs(diff(stats$mu))/SD
  # 
  # print(sprintf("taxon=%s, variable=%s", data_col, variable_name))
  # 
  # if (stats$N[1]>=2 & stats$N[2]>=2)
  # {
  #   power = pwr.t2n.test(
  #     n1=stats$N[1],
  #     n2=stats$N[2],
  #     d=effect_size,
  #     sig.level=0.2
  #   )
  #   print(power)
  # } else
  # {
  #   warning("Insufficient sample size for power calculation.")
  # }

  # print(sprintf('variable_name=%s', variable_name))
  
  ### Allow for labels other than Case/Control
  if (length(labels) != 0)
  {
    case_label = paste0(case, ' (', labels$case, ')')
    control_label = paste0(control, ' (', labels$control, ')')
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
      method='smiley',
      varwidth = T,
      size=3
    ) +
    ### Boxplot part. Split by values of variable_name
    geom_boxplot(
      aes(x=!!as.name(variable_name), y=!!as.name(data_col)),
      alpha=0.2,
      width=0.3,
      fill='grey',
      show.legend = F
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


makeAlphaDivPlot1 = function(
  metadata,
  alpha_div_table,
  indices,
  aesthetics, ### list(x_var='', color_var='', facet_var='')
  additional_title_text='',
  annotation_data=''
)
{
  ### Aesthetics
  ### x_var: Which variable defines the groups along the x-axis
  ### color_var: which variable is used for different color groups (legend)
  ### facet_var: which variable is used to group the plots
  ### index: the variable that has a list of alpha diversity indeces (e.g. shannon, ...)
  
  ### Data for testing
  # additional_title_text = ''
  # metadata = allbac_metadata
  # alpha_div_table = alpha_diversity
  # indices = c('shannon', 'simpson')
  # aesthetics = list(x_var='index', color_var='index', facet_var='')
  # 
  x_var = aesthetics$x_var
  color_var = aesthetics$color_var
  facet_var = aesthetics$facet_var

  print(sprintf("x_var %s, color_var %s, facet_var %s", x_var, color_var, facet_var))
  
  ### Cols containing data that will be used in the plot
  ### The data is grouped by these columns and the rest left out.
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
  
  print("cols to gather:")
  print(cols_to_gather)
  
  plot_data = 
    metadata %>%
    inner_join(alpha_div_table, by='SampleName') %>%
    select(!!cols_to_gather) %>%
    # gather(key='index', value='value', -!!cols_to_not_gather) %>%
    gather(key='index', value='value', indices) %>%
    left_join(annotation_data, by='index')
    
  print('colnames plot_data')
  print(colnames(plot_data))
  # print(colnames(annotation_data))
    
  plt = 
    plot_data %>%
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
    ggtitle(paste('Alpha Diversity:', color_var, '+', x_var, additional_title_text))
  
  print("adding geom text")
  if (annotation_data != '')
  {
    plt = plt + 
      geom_text(
        # data=annotation_data,
        aes(x=xloc, y=yloc, label=pvals, color=index),
        color='black'
      )
  }
  
  if (facet_var != '')
  {
    print("got facet_var 2")
    plt = plt + facet_wrap(
      as.formula(paste('~',facet_var)), 
      scales='free', 
      shrink=F
      )
  }
  
  # print(plt)
  
  return(plt)
}

geom_pval_annotation = function(annotations)
{
  geom_text(
    data=annotation_df,
    aes(x=xloc, y=yloc, label=pvals)
    )
}



makeAlphaDivPlot = function(
  master_table,
  indices,
  aesthetics, ### list(x_var='', color_var='', facet_var='')
  additional_title_text='',
  annotation_data=c()
)
{
  
  ### Aesthetics
  ### x_var: Which variable defines the groups along the x-axis
  ### color_var: which variable is used for different color groups (legend)
  ### facet_var: which variable is used to group the plots
  ### index: the variable that has a list of alpha diversity indeces (e.g. shannon, ...)
  
  ## Data for testing
  # additional_title_text = ''
  # master_table = all_master_table
  # indices = c('shannon', 'simpson')
  # aesthetics = list(x_var='index', color_var='index', facet_var='index')
  # #
  # annotation_data=pval_annotations %>%
  #   filter(TestGroup=='All')

  # additional_title_text = "title text"
  # master_table = temp_master_table
  # indices = c('Firmicutes', 'Bacteroidetes')
  # aesthetics = list(x_var='ARMS2rs10490924', color_var='ARMS2rs10490924', facet_var='index')
  # annotation_data=pval_annotations %>% filter(TestGroup=='AMD_Only')
  # indices=c('Firmicutes', 'Bacteroidetes')
  # aesthetics = list(x_var='CaseString', color_var='CaseString', facet_var='index')
  # annotation_data=pval_annotations %>% filter(TestGroup=='All')
  

  x_var = aesthetics$x_var
  color_var = aesthetics$color_var
  facet_var = aesthetics$facet_var

  print(sprintf("x_var %s, color_var %s, facet_var %s", x_var, color_var, facet_var))
  print(indices)

  
  ### Cols containing data that will be used in the plot
  ### The data is grouped by these columns and the rest left out.
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
  
  print("cols to gather:")
  print(cols_to_gather)
  
  plot_data = 
    master_table %>%
    select(!!cols_to_gather) %>%
    # gather(key='index', value='value', -!!cols_to_not_gather) %>%
    gather(key='index', value='value', indices)

  if (!is.null(annotation_data) && dim(annotation_data)[1] != 0)
  {
    print('filtering annotation data')
    annotations=annotation_data %>% 
      filter(
        index %in% indices, 
        TestVariable==x_var
      )
  }
  
  print('colnames plot_data')
  print(colnames(plot_data))
  # print(colnames(annotation_data))
  print("dim plot data")
  # print(dim(plot_data))
  # print("plot data")
  # print(plot_data)
  # print(class(plot_data))
  # print(typeof(plot_data))
  
  plt = 
    plot_data %>%
    ggplot(aes_string(
      x='index', 
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
        x='index', 
        y='value', 
        color=color_var
      ),
      alpha=0.1,
      # color='black',
      size=0.5,
      width=0.7
    )+
    ggtitle(paste('Alpha Diversity:', color_var, '+', x_var, additional_title_text))
  
  if (dim(annotation_data)[1] != 0 && !is.null(annotation_data))
  {
    print("adding geom text")
    
    plt = plt + 
      geom_text(
        data=annotations,
        aes(x=xloc, y=yloc, label=pvals, color=index),
        color='black'
      )
  }
  
  if (facet_var != '')
  {
    print("got facet_var 2")
    plt = plt + facet_wrap(
      as.formula(paste('~',facet_var)), 
      scales='free', 
      shrink=F
    )
  }
  
  if (additional_title_text != '')
  {
    plt = plt + 
      ggtitle(paste(additional_title_text, name))
  }
  
  plt = plt + ylab("Value") 
  
  plt
  
  # print(plt)
  
  return(plt)
}



plotAllPairTests = function(
    variables,
    master_table,
    indices,
    title_template,
    annotation_data
  )
{
  
  print("plotPairTests")

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
    
    temp_master_table =
      master_table %>%
      filter(!!as.name(name) %in% c(case, control))
    
    plt = makePairPlot(
      master_table=temp_master_table,
      indices=indices,
      var_data=var,
      aesthetics = list(
        x_var=name,
        color_var=name,
        facet_var='index'
      ),
      annotation_data=annotation_data,
      title_template=title_template
    )
    
    print(plt)
  }
}

# indices=c('Firmicutes', 'Bacteroidetes')
# title_template="Firmicutes and Bacteroidetes: AMD Only"
# annotation_data=pval_annotations %>% filter(TestGroup=='AMD_Only')
# variables=amd_only_variables
# # variables = variables['CFHrs10737680']
# master_table = amd_only_master_table
# 
# plotPairTests(
#   variables=variables,
#   master_table=master_table,
#   indices=indices,
#   title_template=title_template,
#   annotation_data=annotation_data
# )



makePairPlot= function(
  master_table,
  indices,
  var_data,
  aesthetics, ### list(x_var='', color_var='', facet_var='')
  title_template='',
  annotation_data=c()
)
{
  
  print("makePairPlot")
  
  # print(var_data)
  # print("annotation_data")
  # print(annotation_data)
  
  ### Aesthetics
  ### x_var: Which variable defines the groups along the x-axis
  ### color_var: which variable is used for different color groups (legend)
  ### facet_var: which variable is used to group the plots
  ### index: the variable that has a list of alpha diversity indeces (e.g. shannon, ...)

  # master_table = fb_master_table
  # indices = raw_exp_vars
  # annotation_data=pval_annotations %>% filter(TestGroup=='AMD_Only')
  # title_template="FB Abundance: "
  # variables=amd_only_variables
  # var_data=var_data=variables$AREDS
  # name = var_data$covariate_of_interest
  # case = var_data$case
  # control = var_data$control
  # aesthetics = list(
  #   x_var=name,
  #   color_var=name,
  #   facet_var='index'
  # )
  
  x_var = aesthetics$x_var
  color_var = aesthetics$color_var
  facet_var = aesthetics$facet_var
  
  name = var_data$covariate_of_interest
  case = var_data$case
  control = var_data$control

  
  print(sprintf("x_var %s, color_var %s, facet_var %s", x_var, color_var, facet_var))
  print(indices)
  
  ### Cols containing data that will be used in the plot
  ### The data is grouped by these columns and the rest left out.
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
  
  plot_data = 
    master_table %>%
    select(!!cols_to_gather) %>%
    # gather(key='index', value='value', -!!cols_to_not_gather) %>%
    gather(key='index', value='value', indices) %>%
    mutate(label=case_when(
      !!as.name(x_var)==case ~ var_data$labels$comparison,
      !!as.name(x_var)==control ~ var_data$labels$reference
    ))
  
  if (!is.null(annotation_data) && dim(annotation_data)[1] != 0)
  {
    print('filtering annotation data')
    annotations=annotation_data %>%
      filter(
        index %in% indices,
        TestVariable==x_var
      )
    # print(annotations)
  }
  
  print('colnames plot_data')
  print(colnames(plot_data))

  plt = 
    plot_data %>%
    ggplot(aes_string(
      x='label', 
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
        x='label', 
        y='value', 
        color=color_var
      ),
      alpha=0.1,
      # color='black',
      size=0.5,
      width=0.7
    )
  
  if (dim(annotation_data)[1] != 0 && !is.null(annotation_data))
  {
    print("adding geom text")
    
    plt = plt + 
      geom_text(
        data=annotations,
        aes(x=xloc, y=yloc, label=pvals, color=index),
        color='black'
      )
  }
  
  if (facet_var != '')
  {
    print("got facet_var 2")
    plt = plt + facet_wrap(
      as.formula(paste('~',facet_var)), 
      scales='free', 
      shrink=F
    )
  }
  
  if (title_template != '')
  {
    plt = plt + 
      ggtitle(paste(title_template, name))
  }
  
  legend_labels = 
    sprintf('(%s) %s', c(case, control), c(var_data$labels$comparison, var_data$labels$reference))
  # print(legend_labels)
  
  plt = plt +
    scale_color_discrete(
      name = name, 
      breaks=c(case, control),
      labels=legend_labels
    ) +
    ylab("Value") +
    xlab(NULL)
  
  plt
  
  # print(plt)
  
  return(plt)
}


plotPairTest = function(
  master_table,
  indices,
  var_data,
  aesthetics, ### list(x_var='', color_var='', facet_var='')
  title_template='',
  annotation_data=c()
)
{
  
  print("makePairPlot")
  
  # print(var_data)
  # print("annotation_data")
  # print(annotation_data)
  
  ### Aesthetics
  ### x_var: Which variable defines the groups along the x-axis
  ### color_var: which variable is used for different color groups (legend)
  ### facet_var: which variable is used to group the plots
  ### index: the variable that has a list of alpha diversity indeces (e.g. shannon, ...)
  
  # master_table = fb_master_table
  # indices = raw_exp_vars
  # annotation_data=pval_annotations %>% filter(TestGroup=='AMD_Only')
  # title_template="FB Abundance: "
  # variables=amd_only_variables
  # var_data=var_data=variables$AREDS
  # name = var_data$covariate_of_interest
  # case = var_data$case
  # control = var_data$control
  # aesthetics = list(
  #   x_var=name,
  #   color_var=name,
  #   facet_var='index'
  # )
  
  x_var = aesthetics$x_var
  color_var = aesthetics$color_var
  facet_var = aesthetics$facet_var
  
  name = var_data$covariate_of_interest
  case = var_data$case
  control = var_data$control
  
  
  print(sprintf("x_var %s, color_var %s, facet_var %s", x_var, color_var, facet_var))
  print(indices)
  
  ### Cols containing data that will be used in the plot
  ### The data is grouped by these columns and the rest left out.
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
  
  plot_data = 
    master_table %>%
    select(!!cols_to_gather) %>%
    # gather(key='index', value='value', -!!cols_to_not_gather) %>%
    gather(key='index', value='value', indices) %>%
    mutate(label=case_when(
      !!as.name(x_var)==case ~ var_data$labels$comparison,
      !!as.name(x_var)==control ~ var_data$labels$reference
    ))
  
  if (!is.null(annotation_data) && dim(annotation_data)[1] != 0)
  {
    print('filtering annotation data')
    annotations=annotation_data %>%
      filter(
        index %in% indices,
        TestVariable==x_var
      )
    # print(annotations)
  }
  
  print('colnames plot_data')
  print(colnames(plot_data))
  
  plt = 
    plot_data %>%
    ggplot(aes_string(
      x='label', 
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
        x='label', 
        y='value', 
        color=color_var
      ),
      alpha=0.1,
      # color='black',
      size=0.5,
      width=0.7
    )
  
  if (dim(annotation_data)[1] != 0 && !is.null(annotation_data))
  {
    print("adding geom text")
    
    plt = plt + 
      geom_text(
        data=annotations,
        aes(x=xloc, y=yloc, label=pvals, color=index),
        color='black'
      )
  }
  
  if (facet_var != '')
  {
    print("got facet_var 2")
    plt = plt + facet_wrap(
      as.formula(paste('~',facet_var)), 
      scales='free', 
      shrink=F
    )
  }
  
  if (title_template != '')
  {
    plt = plt + 
      ggtitle(paste(title_template, name))
  }
  
  legend_labels = 
    sprintf('(%s) %s', c(case, control), c(var_data$labels$comparison, var_data$labels$reference))
  # print(legend_labels)
  
  plt = plt +
    scale_color_discrete(
      name = name, 
      breaks=c(case, control),
      labels=legend_labels
    ) +
    ylab("Value") +
    xlab(NULL)
  
  plt
  
  # print(plt)
  
  return(plt)
}


getPvalAnnotations = function(
    variables,
    pval_height_factor,
    master_table,
    pair_stats
  )
{
  pval_annotations = data.frame(
    index = variables,
    xloc = c(1.5, 1.5, 1.5, 1.5, 1.5),
    yloc = pval_height_factor*master_table %>% 
      select(variables) %>% 
      summarize_all(max) %>% 
      unlist()
  )
  
  pval_annotations =
    pair_stats %>% 
    gather(
      key="index", 
      value='pvals', 
      !!variables
    ) %>%
    left_join(pval_annotations, by='index') %>%
    mutate(pvals = paste0('p = ', round(pvals, 3)))
}

makePairPlot = plotPairTest
plotPairTests = plotAllPairTests


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
  
  plot_data = 
    inner_join(
      pvalues %>% select(Predictor, response_var), 
      effect_sizes %>% select(Predictor, response_var),
      by='Predictor',
      suffix=c('.pvalue', '.effect_size')
      ) %>%
    select(
      Predictor,
      pvalue = !!as.name(paste0(response_var, '.pvalue')),
      effect_size = !!as.name(paste0(response_var, '.effect_size'))
    )
  print(plot_data)
  
  title = paste(title_template, response_var)
  
  plt = 
    plot_data %>%
    ggplot(
      aes(
        x=Predictor, 
        y=effect_size,
        fill=pvalue
    )) +
    geom_bar(stat='identity') +
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

