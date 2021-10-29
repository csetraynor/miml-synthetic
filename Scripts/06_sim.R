rm(list = ls())
require(simtte)
require(mice)
require(readr)
require(purrr)
require(dplyr)
library(simstandard)
library(lavaan)
library(survival)
require(miceadds)
require(purrr)

cor_level <- "high_cor"
N <- 1e4
S <- 1:25

set.seed(123456)
simdata <- S %>% map(function(s){
  parentDir <- file.path("DerivedData/simulations/numeric", cor_level, N, s)
  simdata <- read_rds(file.path(parentDir, "simdata.RDS"))
  
  beta <- c(-2, -1, 0, 1, 2, 3)
  lp <- as.matrix(simdata) %*% beta
  
  res <- sim_tte(pi = lp,
                 mu = -1, 
                 coefs = 1.1,
                 type = "weibull",
                 obs.only = F,
                 obs.aug = T,
                 delta = 0.05,
                 end_time = 500)
  return(loo::nlist(res, simdata))
})

saveRDS(simdata, "DerivedData/simulations/numeric/simttedata.RDS")


cor_level <- "low_cor"
N <- 1e4
S <- 1:25

set.seed(123456)
require(furrr)
plan(multisession, workers = 4)

simdata <- S %>% future_map(function(s){
  parentDir <- file.path("DerivedData/simulations/numeric", cor_level, N, s)
  simdata <- read_rds(file.path(parentDir, "simdata.RDS"))
  
  beta <- c(-2, -1, 0, 1, 2, 3)
  lp <- as.matrix(simdata) %*% beta
  
  res <- sim_tte(pi = lp,
                 mu = -1, 
                 coefs = 1.1,
                 type = "weibull",
                 obs.only = F,
                 obs.aug = T,
                 delta = 0.05,
                 end_time = 500)
  return(loo::nlist(res, simdata))
})

saveRDS(simdata, "DerivedData/simulations/numeric/simttedata_lowcor.RDS")
