library(readr)
library(dplyr)

pca_impute <- function(mis_data, M = 5) {
  
  require(missMDA)
  require(FactoMineR)
  ## run the missMDA algorithm
  ## ds location of data with missing values
  ## ncp number of components
  nb <- estim_ncpPCA(as.matrix(mis_data), ncp.max=4)
  nb <- nb$ncp
  if(nb < 1) nb <- 1 ## variability not allows to use nb = 0
  imp <- MIPCA(as.matrix(mis_data), ncp=nb, nboot = M)
  require(mice)
  imp <- prelim(res.mi = imp, X = mis_data)
  return(imp)
}

res <- pca_impute(mis_data, M = M)