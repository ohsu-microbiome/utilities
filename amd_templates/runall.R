# runall.R

library(rmarkdown)


files = c(
  'miseq-239_FB_data_prep.Rmd',
  'miseq-239_FB_pair_test.Rmd',
  'miseq-239_FB_plots.Rmd',
  'miseq-239_FB_regression.Rmd'
)

for (file in files)
{
  render(file)
}