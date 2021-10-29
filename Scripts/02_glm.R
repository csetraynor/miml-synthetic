library(readr)
library(dplyr)

glm_impute <- function(mis_data, M = 5)  {
  ## load dev mice
  require(mice)
  imp <- mice(mis_data, m = M, maxit = 5, defaultMethod = c("pmm", "logreg", "polyreg", "polr"))
  return(imp)
}

res <- glm_impute(mis_data, M = M)