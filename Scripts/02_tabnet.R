library(readr)
library(dplyr)

tabnet_impute <- function(mech, prop, boot, type, M = 20, dropout = 0.1, gpu = T) {
  
  foo <- file.path("DerivedData/rwd/amputated", mech, prop, boot, type)
  mis_data <- read_csv(file.path( foo, "amputed_data.csv") )
  mis_data <- mis_data %>%
    mutate_at(c("ALK", "EGFR", "KRAS", "BRAF", "PDL1", "event_status"), ~ as.factor( as.character(.)) )
  ## load dev mice
  devtools::document("mice")
  
  imp <- mice(mis_data,
              m = M,
              maxit = 3,
              method = "tabnetClassifier",
              gpu = F,
              hpc = F,
              dropout = dropout,
              embedding_cols = colnames(mis_data %>% select_if(is.factor)))
  
  resPath <- file.path("Results/rwd/amputated", mech, prop, boot, type, "tabnet")
  
  if(!dir.exists(resPath)) dir.create(resPath, recursive = TRUE)
  saveRDS(imp, file.path(resPath, "imp.RDS") )
  
  return(0)
}
