library(tidyverse)
library(magrittr)


getAllInteractionModels = function(
  base_predictors,
  outcome_variable
)
{
  interactions = apply(combn(base_predictors,3), 2, function(x) paste0(x, collapse=":"))

  rhs_models = c("1")

  base_formulas = c()
  for (n in 1:length(base_predictors))
  {
    base_combinations_n = combn(base_predictors, n)
    base_formulas_n = apply(base_combinations_n, 2, function(x) paste0(x, collapse=" + "))
    # print(base_formulas_n)
    base_formulas = c(base_formulas, base_formulas_n)
  }
  rhs_models = c(rhs_models, base_formulas)

  makeRegexPattern = function(c)
  {
    sprintf("(?=.*%s)", c) %>% paste0(., collapse="") %>% paste0("^", .)
  }

  for (n in 1:length(base_predictors))
  {
    # n=4
    interaction_combinations = combn(interactions, n)
    # print(interaction_combinations)
    interaction_formulas = apply(interaction_combinations, 2, function(x)
    {
      just_interactions_formula = paste0(x, collapse=" + ")

      required_base_predictors =
        str_match(just_interactions_formula, base_predictors) %>%
        unlist() %>%
        .[!is.na(.)]

      pat = makeRegexPattern(required_base_predictors)

      base_predictor_combination_indeces = grep(pat, base_formulas, perl=T)
      base_predictor_combination_formulas = base_formulas[base_predictor_combination_indeces]

      hybrid_formulas = paste0(base_predictor_combination_formulas, " + ", just_interactions_formula)

      hybrid_formulas
    })

    rhs_models = c(rhs_models, interaction_formulas) %>% unlist()
  }

  full_models = sapply(
    rhs_models,
    function(x) paste(outcome, "~", x),
    simplify=T,
    USE.NAMES = F
  )

  return(full_models)
}


all = getAllInteractionModels(c("A", "B", "C", "D"), "response")

all
