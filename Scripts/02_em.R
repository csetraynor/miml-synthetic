library(readr)
library(dplyr)

em_impute <- function(mis_data, M = 5) {
  require(Amelia)
  imp <- amelia(as.data.frame(mis_data), 
                m = M, 
                # noms = nominal_vars, 
                incheck = TRUE)
  return(imp)
}

res <- em_impute(mis_data, M = M)