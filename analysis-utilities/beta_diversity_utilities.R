library(tidyverse)
library(magrittr)
library(ggplot2)
library(plotly)


makeNMDSPlot = function(
  master_table,
  features,
  axes=c(1,2),
  color,
  title_extra='',
  elipses=T
)
{
  community_matrix =
    master_table %>%
    select(SampleName, features) %>%
    column_to_rownames('SampleName') %>%
    as.matrix()

  nmds_data = metaMDS(community_matrix, k=max(axes), engine='monoMDS', autotransform=T)

  mds_axes = paste0('MDS', axes)

  plt =
    nmds_data$points %>%
    data.frame() %>%
    rownames_to_column('SampleName') %>%
    inner_join(master_table, by='SampleName') %>%
    ### plotting
    ggplot(aes_string(x=mds_axes[[1]], y=mds_axes[[2]], color=color)) +
    geom_point() +
    ggtitle(paste('nMDS', title_extra)) +
    scale_color_manual(values=c('red', 'blue'))

  if (elipses)
  {
    plt = plt + stat_ellipse()
  }

  print(plt)

  return(plt)
}


makePCOAPlot = function(
  master_table,
  method,
  color,
  features,
  axes=c(1,2),
  title_extra='',
  elipses=T
)
{
  # sample_taxa_table = master_table %>% filter(IGA %in% c('Neg', 'Pos'))
  # method = 'bray'
  # color = 'IGA'
  # features = filtered_taxa

  ### For title
  capitalized_method = paste0(toupper(substr(method, 1, 1)), substr(method, 2, nchar(method)))
  axes_cols = paste0('PC', axes)

 community_matrix =
    master_table %>%
    column_to_rownames('SampleName') %>%
    select(features)

  distances =
    vegdist(community_matrix, diag=T, upper=T, method='bray') %>%
    as.matrix() %>%
    as.data.frame()

  principal_components =
    prcomp(distances) %>%
    summary() %>%
    .$x %>%
    data.frame() %>%
    select(axes_cols) %>%
    rownames_to_column('SampleName') %>%
    inner_join(master_table, by='SampleName')

  plt =
    principal_components %>%
    ggplot(aes_string(x=axes_cols[[1]], y=axes_cols[[2]], color=color)) +
    geom_point() +
    ggtitle(sprintf("PCoA %s Distance %s", capitalized_method, title_extra)) +
    scale_color_manual(values=c('red', 'blue'))


  if (elipses)
  {
    plt = plt + stat_ellipse()
  }

  print(plt)

  return(plt)
}


plotUnifracPCoA = function(
  ps_object,
  subset_string="",
  ord_type="PCoA",
  dist_type="UNIFRAC",
  weighted=FALSE,
  plot_type='samples',
  color="",
  axes=1:2
)
{
  ps_object=ps
  subset_string=""
  ord_type="PCoA"
  dist_type="UNIFRAC"
  weighted=T
  plot_type="samples"
  color="CaseString"
  axes=c(1,2)

  subset_string = ifelse(subset_string=="", 'NONE', subset)
  weighted_string = ifelse(weighted, 'WEIGHTED', 'UNWEIGHTED')

  title_string = paste(
    "subset: ", subset_string, " ",
    "ord: ", toupper(ord_type), " ",
    "dist:", toupper(dist_type), "+", weighted_string, " ",
    "dim: ", toupper(plot_type)
  )

  print(sprintf('plot_type: %s', plot_type))

  ps_ord = ordinate(ps_object, ord_type, dist_type, weighted=weighted)
  plot_ordination(
    ps_object,
    ps_ord,
    type=plot_type,
    color=color,
    title=title_string,
    axes=axes
  )  +
    scale_color_manual(values=c('red', 'blue'))
}

plotUnifracOrd = function(
  ps_object,
  ordination,
  ord_type="PCoA",
  dist_type="UNIFRAC",
  weighted=FALSE,
  plot_type='samples',
  var_data,
  color="",
  axes=c(1,2),
  subset_string=""
)
{
  ### Data for Testing
  # ps_object=ps
  # subset_string=""
  # ord_type="PCoA"
  # dist_type="UNIFRAC"
  # weighted=F
  # plot_type="samples"
  # color="CaseString"
  # axes=c(1,2)
  # # ps_ord = ordinate(ps_object, ord_type, dist_type, weighted=weighted)
  # ordination=ps_ord

  if (!is.null(var_data))
  {
    name = var_data$covariate_of_interest
    case = var_data$case
    control = var_data$control
    reference = var_data$labels$reference
    comparison = var_data$labels$comparison

  } else
  {
    name = ""
    case = ""
    control = ""
    reference = ""
    comparison = ""
  }
  print(sprintf("name %s, case %s, control %s", name, case, control))

  weighted_string = ifelse(weighted, 'WEIGHTED', 'UNWEIGHTED')
  print(weighted_string)

  title_string = paste(
    "subset: ", subset_string, " ",
    "ord: ", toupper(ord_type), " ",
    "dist:", toupper(dist_type), "+", weighted_string, " ",
    "dim: ", toupper(plot_type)
  )

  print(sprintf('plot_type: %s', plot_type))

  plt = plot_ordination(
    ps_object,
    ordination,
    type=plot_type,
    color=color,
    title=title_string,
    axes=axes
  ) +
    theme(plot.title = element_text(size = 9, face = "bold"))

  if (!is.null(var_data))
  {
    legend_labels = sprintf(
      '(%s) %s',
      c(case, control),
      c(var_data$labels$comparison, var_data$labels$reference)
    )

    # print(legend_labels)

    plt = plt +
      scale_color_manual(
        name = name,
        breaks=c(case, control),
        labels=legend_labels,
        values=c('red', 'blue')
      )
  }
}

getNMDS = function(
  master_table,
  features,
  id_col='SampleName',
  axes=c(1,2)
)
{
  community_matrix =
    master_table %>%
    select(!!id_col, features) %>%
    column_to_rownames(id_col) %>%
    as.matrix()

  nmds_data = metaMDS(
    community_matrix,
    k=max(axes),
    engine='monoMDS',
    autotransform=T
    )

  return(nmds_data)
}


getPrinceComps = function(
  master_table,
  features,
  id_col='SampleName',
  method='bray'
)
{
  ### Data for testing
  # master_table = master_table
  # features = filtered_taxa
  # id_col = "SampleName"
  # method='bray'

  community_matrix =
    master_table %>%
    column_to_rownames(id_col) %>%
    select(features)

  distances =
    vegdist(
      community_matrix,
      diag=T,
      upper=T,
      method=method
      ) %>%
    as.matrix() %>%
    as.data.frame()

  principal_components =
    prcomp(distances)

  return(principal_components)
}

plotUnifracPCoA = function(
  ps_object,
  subset="",
  ord_type="PCoA",
  dist_type="UNIFRAC",
  weighted=FALSE,
  plot_type='samples',
  color="",
  axes=1:2
)
{
  subset_string = ifelse(subset=="", 'NONE', subset)
  weighted_string = ifelse(weighted, 'WEIGHTED', 'UNWEIGHTED'
  )
  title_string = paste(
    "subset: ", subset_string, " ",
    "ord: ", toupper(ord_type), " ",
    "dist:", toupper(dist_type), "+", weighted_string, " ",
    "dim: ", toupper(plot_type)
  )

  print(sprintf('plot_type: %s', plot_type))

  ps_ord = ordinate(ps_object, ord_type, dist_type, weighted=weighted)
  plot_ordination(
    ps_object,
    ps_ord,
    type=plot_type,
    color=color,
    title=title_string,
    axes=axes
  ) +
    scale_color_manual(values=c('red', 'blue'))
}

plotOrd3D = function(
  princomp_master_table,
  axes=c('PC1', 'PC2', 'PC3'),
  id_col="SampleName",
  color="",
  title_extra=""
)
{
  axx <- list(
    gridcolor='rgb(255, 255, 255)',
    zerolinecolor='rgb(255, 255, 255)',
    showbackground=TRUE,
    backgroundcolor='rgb(230, 230,230)'
  )

  mmargin <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 0
  )

  plt = plot_ly(
    data=princomp_master_table,
    x = ~get(axes[1]),
    y = ~get(axes[2]),
    z = ~get(axes[3]),
    color=~get(color),
    colors=c('red', 'blue', 'green')
  ) %>%
    add_markers(size=3) %>%
    layout(
      title = sprintf("Principal Components %s", title_extra),
      scene = list(
        aspectmode='cube',
        xaxis=c(axx, title=axes[1]),
        yaxis=c(axx, title=axes[2]),
        zaxis=c(axx, title=axes[3])
      ),
      paper_bgcolor = 'rgb(243, 243, 243)',
      plot_bgcolor = 'rgb(243, 243, 243)',
      margin=margin
    )

  # print(plt)

  return(plt)

}
