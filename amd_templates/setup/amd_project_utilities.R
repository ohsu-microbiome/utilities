
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


# getCurrentFileLocation <-  function()
# {
#   this_file <- commandArgs() %>%
#     enframe(name = NULL) %>%
#     separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
#     filter(key == "--file") %>%
#     pull(value)
#   if (length(this_file)==0)
#   {
#     this_file <- rstudioapi::getSourceEditorContext()$path
#   }
#   return(dirname(this_file))
# }


generateSourceFile = function(
  template_type = '',
  analysis_type,
  src_dir='.'
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
  #   src_dir = file.path(project_root, 'analysis', analysis_type)
  # } else
  # {
  #   src_dir = file.path(project_root, 'analysis', analysis_type, clustering_level)
  # }
  #
  # dir = file.path(src_dir, 'src')

  print(sprintf('template_type: %s', template_type))
  print(sprintf('src_dir: %s', src_dir))

  analysis_dir = gsub('(.*/).*$', '\\1', src_dir)
  print(sprintf("one level up dir: %s", analysis_dir))

  tables_dir = file.path(analysis_dir, 'tables')
  print(sprintf("tables dir: %s", tables_dir))

  # project_metadata_file = file.path(project_root, "metadata", "project_metadata.R")
  # print(sprintf("project metadata file: %s", project_metadata_file))
  # source(project_metadata_file)
  #
  analysis_metadata_file = file.path(src_dir, "analysis_metadata.R")
  print(sprintf("Analysis  metadata file: %s", analysis_metadata_file))
  source(analysis_metadata_file)

  templates_dir = file.path(utilities_dir, 'amd_templates/analysis')
  print(sprintf("templates dir: %s", templates_dir))

  template_filename = paste(template_type, 'template.Rmd', sep='_')
  # print(sprintf("template filename: %s", template_filename))

  template_file = file.path(templates_dir, template_filename)
  print(sprintf("template file: %s", template_file))

  template_text =
    readLines(template_file) %>%
    paste(collapse="\n") %>%
    as.character() %>%
    str_interp()

  new_source_file = file.path(
    src_dir,
    paste0(
      miseq_project_prefix, "_",
      analysis_type, "_",
      gsub("_template", "", template_filename)
    )
  )
  print(sprintf('new source file name: %s', new_source_file))

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




makeAnalysisSubtitle = function(starting_dir)
{
  print('makeAnalysisSubtitle')
  # print(starting_dir)
  parts = strsplit(starting_dir, '/')[[1]]
  # print(parts)
  analysis_index = which(parts=='analysis')
  # print(analysis_index)
  # print(parts[analysis_index])
  # print(length(parts))
  # print(parts[length(parts)-1])
  start = analysis_index + 1
  end = length(parts) - 1
  print(sprintf('start: %s  end: %s', start, end))
  print(sprintf('start: %s  end: %s', parts[start], parts[end]))
  print('subtitle parts')
  subtitle_parts = parts[start:end]
  # print(subtitle_parts)
  analysis_subtitle = paste(subtitle_parts, collapse=' : ')
  print(analysis_subtitle)

  return(analysis_subtitle)
}

