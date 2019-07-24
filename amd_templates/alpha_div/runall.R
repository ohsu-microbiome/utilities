# runall.R

library(rmarkdown)


files = c(
  'miseq-239_alpha_div_data_prep.Rmd',
  'miseq-239_alpha_div_pair_tests.Rmd',
  'miseq-239_alpha_div_pair_plots.Rmd',
  'miseq-239_alpha_div_regression.Rmd',
  'miseq-239_alpha_div_regression_plots.Rmd'
)

for (file in files)
{
  render(file)
}
