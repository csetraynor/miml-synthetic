library(readr)
library(dplyr)

rf_impute <- function(mis_data, M = 5) {
  
  ## load dev mice
  require(mice)
  imp <- mice(mis_data, m = M, maxit = 5, method = "rf")
  
  return(imp)
}

res <- rf_impute(mis_data, M = M)