# plotTwoGroupTest = function(
#   master_table,
#   indices,
#   var_data,
#   aesthetics, ### list(x_var='', color_var='', facet_var='')
#   title_template='',
#   annotation_data=c()
# )
#
# makeTwoGroupPlot= function(
#   master_table,
#   indices,
#   var_data,
#   aesthetics, ### list(x_var='', color_var='', facet_var='')
#   title_template='',
#   annotation_data=c()
# )
#
# makeAlphaDivPlot = function(
  # master_table,
  # indices,
  # aesthetics, ### list(x_var='', color_var='', facet_var='')
  # additional_title_text='',
  # annotation_data=c()
# )


makeBoxAndDotplot = function(
  master_table,
  indices,
  var_data,
  aesthetics, ### list(x_var='', color_var='', facet_var='')
  annotation_data=c(),
  include=c("dots", "box"),
  title=NULL,
  xlabel=NULL,
  ylabel=NULL
)
{
  name = var_data$covariate_of_interest
  case = var_data$case
  control = var_data$control

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

  plot_data =
    master_table %>%
    select(!!cols_to_gather) %>%
    # gather(key='index', value='value', -!!cols_to_not_gather) %>%
    gather(key='index', value='value', indices) %>%
    mutate(label=case_when(
      !!as.name(x_var)==case ~ var_data$labels$comparison,
      !!as.name(x_var)==control ~ var_data$labels$reference,
      TRUE ~ index
    ))

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
    print("got facet_var")
    plt = plt + facet_wrap(
      as.formula(paste('~',facet_var)),
      scales='free',
      shrink=F
    )
  }

  if (1==1)
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



