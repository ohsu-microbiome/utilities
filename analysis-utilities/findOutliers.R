library(tidyverse)
library(magrittr)

# ```{r}
# out = ols_step_all_possible(all)
#
# res =
#   out %>%
#   arrange(-adjr) %>%
#   select(mindex, predictors, adjr) %>%
#   head(5) %>%
#   full_join(
#     out %>%
#       arrange(cp) %>%
#       select(mindex, predictors, cp) %>%
#       head(5)
#   ) %>%
#   full_join(
#     out %>%
#       arrange(aic) %>%
#       select(mindex, predictors, aic) %>%
#       head(5)
#   ) %>%
#   full_join(
#     out %>%
#       arrange(sbic) %>%
#       select(mindex, predictors, sbic) %>%
#       head(5)
#   )
# ```

getDiagnosticCutoffs = function(model)
{
  num_predictors =
    model$call$formula %>%
    as.character() %>% .[[3]] %>%
    str_split(" \\+ ") %>% .[[1]] %>%
    length()

  n =
    model$residuals %>%
    length()

  dffits_cutoff = sqrt((num_predictors+1)/n)
  dfbetas_cutoff = 2/sqrt(n)
  hat_cutoff = 2*(num_predictors+1)/n
  cooks_distance_cutoff = 4/n

  diagnostic_cutoffs = list(
    dffit = dffits_cutoff,
    dfbetas = dfbetas_cutoff,
    hat = hat_cutoff,
    cooks_dist = cooks_distance_cutoff
  )

  return(diagnostic_cutoffs)
}


findOutliers = function(model)
{
  dc = getDiagnosticCutoffs(model)

  print("***  Diagnostic Cutoffs  ***")
  print(dc)

  im =
    influence.measures(model) %>%
    .$infmat %>%
    data.frame() %>%
    mutate(obs_number=row_number()) %>%
    select(-dfb.1_, -cov.r) %>%
    select(obs_number, everything())

  dfbeta_columns = colnames(im) %>% grep("dfb", ., value=T)

  outliers =
    im %>%
    select(obs_number, hat, cook.d, dffit, starts_with("dfb.")) %>%
    mutate(
      hat = ifelse(hat<dc$hat, NA, hat),
      cook.d = ifelse(cook.d < dc$cooks, NA, cook.d),
      dffit = ifelse(dffit < dc$dffit, NA, dffit)
    )

  for (col in dfbeta_columns)
  {
    outliers %<>% mutate(
      !!as.name(col) := ifelse(!!as.name(col) < dc$dfbetas, NA, !!as.name(col))
    )
  }

  outliers %<>%
    filter_at(vars(-obs_number), any_vars(!is.na(.))) %>%
    mutate_all(~signif(., 3))

  return(outliers)

}
