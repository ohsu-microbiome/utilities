startCondaEnv('igaseq')

library(rmarkdown)
library(stringr)

path_to_source_files = '~/miseq-278/analysis/igaseq/amd_only/src'

# prepare_igaseq_rmd_source = file.path(path_to_source_files, 'prepare_igaseq_amd_only.Rmd')
# print(sprintf("prepare rmd source=%s", prepare_igaseq_rmd_source))
# analyze_igaseq_rmd_source = file.path(path_to_source_files, 'analyze_igaseq_amd_only.Rmd')
# print(sprintf("analyze rmd source=%s", analyze_igaseq_rmd_source))

prepare_template_file = file.path(path_to_source_files, 'prepare_igaseq_amd_only_template.Rmd')
prepare_template_text = 
  paste(readLines(prepare_template_file), collapse="\n")  %>% 
  as.character()
analyze_template_file = file.path(path_to_source_files, 'analyze_igaseq_amd_only_template.Rmd')
analyze_template_text = 
  paste(readLines(analyze_template_file), collapse="\n") %>% 
  as.character()

variables_of_interest = list(
  # list(
  #   covariate_of_interest='test_cov',
  #   case='test_case',
  #   control='test_control'
  # ),
  list(
    covariate_of_interest='ARMS2_rs10490924',
    control='GG',
    case='TT'
    ),
  list(
    covariate_of_interest='CFH_rs1061170',
    control='TT',
    case='CC'
    ),
  # list(covariate_of_interest='CFH_rs10737680', control='CC', case='AA')
  list(
    covariate_of_interest='GA_No_CNV_Either_Eye',
    control=0,
    case=1
    ),
  list(
    covariate_of_interest='CNV_Either_Eye',
    control=0,
    case=1
    ),
  list(
    covariate_of_interest='AREDS',
    control='N',
    case='Y'
    ),
  list(
    covariate_of_interest='SKIV2L_rs429608',
    control='AA',
    case='GG'
    )
  )


for (study_params in variables_of_interest)
{

  # study_params = variables_of_interest[[1]]
  print(study_params)
  
  COVARIATE_OF_INTEREST=study_params$covariate_of_interest
  CASE=study_params$case
  CONTROL=study_params$control
  
  print(sprintf("covariate of interest: %s, case: %s, control: %s",
                COVARIATE_OF_INTEREST,
                CASE,
                CONTROL)
        )
  working_directory = file.path("~/miseq-278/analysis/igaseq/amd_only", COVARIATE_OF_INTEREST)
  study_params$working_directory = working_directory
  
  print(sprintf("working_directory: %s", working_directory))
  dir.create(
    working_directory, 
    recursive = T, 
    showWarnings = F
  )
  src_directory = file.path(working_directory, 'src')
  print(sprintf("src_directory: %s", src_directory))
  dir.create(
    src_directory, 
    recursive = T, 
    showWarnings = F
  )
  results_directory = file.path(working_directory, 'results')
  print(sprintf("results_directory: %s", results_directory))
  dir.create(
    results_directory, 
    recursive = T, 
    showWarnings = F
  )
  reports_directory = file.path(working_directory, 'reports')
  print(sprintf("reports_directory: %s", reports_directory))
  dir.create(
    results_directory, 
    recursive = T, 
    showWarnings = F
  )
  
  ### Create RMDs from template
  prepare_rmd_text = str_interp(prepare_template_text, study_params)
  analyze_rmd_text = str_interp(analyze_template_text, study_params)
  
  prepare_igaseq_rmd_file = file=file.path(
    src_directory, 
    paste0('prepare_igaseq_amd_only_', COVARIATE_OF_INTEREST, '.Rmd')
  )
  print(prepare_igaseq_rmd_file)
  analyze_igaseq_rmd_file=file.path(
    src_directory, 
    paste0('analyze_igaseq_amd_only_', COVARIATE_OF_INTEREST, '.Rmd')
  )
  print(analyze_igaseq_rmd_file)
    
  cat(prepare_rmd_text, file=prepare_igaseq_rmd_file)
  cat(analyze_rmd_text, file=analyze_igaseq_rmd_file)
  
  render(
    prepare_igaseq_rmd_file,
    output_dir=reports_directory,
    output_file=paste0('prepare_', COVARIATE_OF_INTEREST, '.nb.html')
  )
  
  render(
    analyze_igaseq_rmd_file,
    output_dir=reports_directory,
    output_file=paste0('analyze_', COVARIATE_OF_INTEREST, '.nb.html')
  )
}


# render(
#   '~/miseq-278/analysis/igaseq/genus_level/src/prepare_igaseq_dev.Rmd', 
#   params=list(COVARIATE_OF_INTEREST=variables_of_interest[1])
#   )

# prepare_template_text = paste(readLines('prepare_igaseq_amd_only_template.Rmd'), collapse="\n")
# prepare_template_text = as.character(prepare_template_text)
# test_prepare_rmd_text = str_interp(prepare_template_text, l)
# cat(test_prepare_rmd_text, file="test_prepare.Rmd")

