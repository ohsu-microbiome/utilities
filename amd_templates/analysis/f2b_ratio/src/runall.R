# runall.R

library(rmarkdown)


files = c(
  'miseq-239_FB_data_prep.Rmd',
  'miseq-239_FB_pair_tests.Rmd',
  'miseq-239_FB_pair_plots.Rmd',
  'miseq-239_FB_regression.Rmd',
  'miseq-239_FB_regression_plots.Rmd'
)

for (file in files)
{
  render(file)
}
