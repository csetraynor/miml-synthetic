library(readr)
library(dplyr)

range_impute <- function(mis_data, M = 5) {
  require(missRanger)
  imp <- seq_len(M) %>% purrr::map(function(m){
    missRanger(mis_data, seed = 2021 + m)
  })
  
  return(imp)
}

res <- range_impute(mis_data, M = M)

