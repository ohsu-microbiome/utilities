setwd("C:\\Users\\ariel\\OneDrive\\Documents\\utilities\\amd_templates\\analysis")

filenames = list.files(pattern="*.Rmd")


for( f in filenames )
{
  print(f)
  x <- readLines(f)

  # x = "source('~/utilities/amd_templates/setup/amd_project_utilities.R')"

  y <- str_remove(
    x,
    "source\\('~\\/utilities\\/amd_templates\\/setup\\/amd_project_utilities\\.R'\\)"
    )

  # y <- gsub(
  #   "~\\/utilities\\/analysis-utilities\\/",
  #   "https://raw.githubusercontent.com/ohsu-microbiome/utilities/master/analysis-utilities/",
  #   x
  #   )
  cat(y, file=f, sep="\n")

}
