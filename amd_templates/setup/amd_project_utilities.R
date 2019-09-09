
findProjectRoot = function(starting_dir, target)
{
  project_root_found = F
  search_dir = file.path(starting_dir, 'dummy')

  repeat
  {
    search_dir = gsub("\\/[a-zA-Z0-9\\-_\\.]+$", "", search_dir)
    print(sprintf("search directory: %s", search_dir))
    files = list.files(path=search_dir, all.files=T)
    print(files)

    project_root_found = ".proj_root" %in% files
    if (project_root_found){break}
  }
  project_root = search_dir
  print(sprintf('project root found: %s', project_root))

  return(project_root)
}


generateSourceFile = function(
  template_type = '',
  analysis_dir='.'
  )
{
  print("generateSourceFile")

  # # project_root = findProjectRoot(getwd(), '.proj_root')
  # miseq_project_prefix = gsub('.*/', '', project_root)
  #
  # print(miseq_project_prefix)
  # print(clustering_level)
  #
  # if (clustering_level == '')
  # {
  #   analysis_dir = file.path(project_root, 'analysis', analysis_type)
  # } else
  # {
  #   analysis_dir = file.path(project_root, 'analysis', analysis_type, clustering_level)
  # }
  #
  # dir = file.path(analysis_dir, 'src')

  print(sprintf('template_type: %s', template_type))
  print(sprintf('analysis_dir: %s', analysis_dir))

  one_level_up_dir = gsub('(.*/).*$', '\\1', analysis_dir)
  print(sprintf("one level up dir: %s", one_level_up_dir))

  tables_dir = file.path(one_level_up_dir, 'tables')
  print(sprintf("tables dir: %s", tables_dir))

  # project_metadata_file = file.path(project_root, "metadata", "project_metadata.R")
  # print(sprintf("project metadata file: %s", project_metadata_file))
  # source(project_metadata_file)

  templates_dir = file.path(utilities_dir, 'amd_templates/analysis')
  print(sprintf("templates dir: %s", templates_dir))

  template_filename = paste(template_type, 'template.Rmd', sep='_')
  print(sprintf("template filename: %s", template_filename))

  template_file = file.path(templates_dir, template_filename)
  print(sprintf("template file: %s", template_file))

  template_text =
    readLines(template_file) %>%
    paste(collapse="\n") %>%
    as.character() %>%
    str_interp()

  new_source_file = file.path(
    analysis_dir,
    paste0(
      miseq_project_prefix, "_",
      gsub("_template", "", template_filename)
    )
  )
  print(sprintf('new source file name: %s',new_source_file))

  writeLines(
    template_text,
    new_source_file
  )

  return(new_source_file)
}

makeDataFileName = function(
  table_contents,
  tables_dir,
  analysis_type,
  miseq_project_prefix,
  clustering_level=NULL
)
{
  filename = file.path(
    tables_dir,
    paste(
      miseq_project_prefix,
      analysis_type,
      clustering_level,
      'clust',
      table_contents,
      sep='_'
    )
  )

  print(sprintf('filename created: %s', filename))
  return(filename)
}

makeAnalysisTitle = function(analysis_type)
{
  analysis_title =
    analysis_type %>%
    strsplit('_') %>%
    .[[1]] %>%
    tools::toTitleCase() %>%
    paste(collapse=' ')

  return(analysis_title)
}
