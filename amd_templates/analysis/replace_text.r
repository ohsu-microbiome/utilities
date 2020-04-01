# setwd("C:\\Users\\ariel\\OneDrive\\Documents\\utilities\\amd_templates\\analysis")
setwd("~/utilities/amd_templates//analysis")


filenames = list.files(pattern="*.Rmd")

for( f in filenames )
{
  print(f)
  x <- readLines(f)

  # x = "source('${utilities_dir}/analysis-utilities/amd_project_utilities.R')"

  y <- gsub(
    "analysis-utilities\\/amd_project_utilities\\.R",
    "amd_templates/setup/amd_project_utilities.R",
    x
    )

  # print(y)

  cat(y, file=f, sep="\n")

}
