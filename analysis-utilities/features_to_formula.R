list_of_features = LETTERS

formula =
  sapply(
    list_of_features,
    ### Wrap features with "rank()"
    function(x){sprintf('rank(%s)',x)},
    USE.NAMES = F
  ) %>%
  ### put commas between them
  paste0(., collapse=",") %>%
  ### wrap in "cbind()" on LHS of a formula
  paste0('cbind(', ., ') ~ CaseString') %>%
  ### turn into formula
  as.formula()

formula
