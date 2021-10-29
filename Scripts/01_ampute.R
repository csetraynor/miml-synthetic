rm(list = ls())
require(mice)
require(readr)
require(purrr)
require(dplyr)
library(simstandard)
library(lavaan)

parentDir <- "DerivedData/simulations"
if(!dir.exists(parentDir)) dir.create(parentDir, recursive = T)

sample_data_sim_high_cor <- function(s, N) {
  # lavaan syntax for model
  m <- "
  A =~ 0.7 * A1 + 0.9 * A2 + 0.9 * A3 + 0.3 * B1
  B =~ 0.7 * B1 + 0.9 * B2 + 0.9 * B3
  B ~ 0.9 * A
  "
  # Simulate data
  d <- sim_standardized(m,
                        n = N,
                        latent = FALSE,
                        errors = FALSE)
  
  parentDir <- file.path("DerivedData/simulations/numeric/high_cor", N, s)
  if(!dir.exists(parentDir)) dir.create(parentDir, recursive = T)
  saveRDS(d, file.path(parentDir, "simdata.RDS"))
}

sample_data_sim_low_cor <- function(s, N) {
  # lavaan syntax for model
  m <- "
  A =~ 0.4 * A1 + 0.4 * A2 + 0.4 * A3 + 0.3 * B1
  B =~ 0.4 * B1 + 0.5 * B2 + 0.4 * B3
  B ~ 0.5 * A
  "
  # Simulate data
  d <- sim_standardized(m,
                        n = N,
                        latent = FALSE,
                        errors = FALSE)
  
  parentDir <- file.path("DerivedData/simulations/numeric/low_cor", N, s)
  if(!dir.exists(parentDir)) dir.create(parentDir, recursive = T)
  saveRDS(d, file.path(parentDir, "simdata.RDS"))
}

## sample sequence of sample size
N_seq <- bsts::GeometricSequence(5, 1e4, .5)
s_seq <- 1:25

N_seq %>% walk(function(N){
  s_seq %>% walk(function(s) {
    sample_data_sim_high_cor(s, N)
    sample_data_sim_low_cor(s, N)
  })
})

ampute_sim <- function(N, s, boot, prop, cor_level, mech) {
  parentDir <- file.path("DerivedData/simulations/numeric", cor_level, N, s)
  d <- read_rds(file.path(parentDir, "simdata.RDS"))
  d_miss <- mice::ampute(d, prop = prop, mech = mech)
  
  # writeLines(paste0("Percentage of newly generated missing values: ", 100*sum(is.na(d_miss$amp))/prod(dim(d_miss$amp)), " %"))
  
  resPath <- file.path("DerivedData/simulations/numeric", cor_level, N, s, prop, boot, mech)
  if(!dir.exists(resPath)) dir.create(resPath, recursive = T)
  saveRDS(d_miss, file.path(resPath, "amputed_obj.RDS"))
  vroom::vroom_write(d_miss$amp, file.path(resPath, "amputed_data.csv"), ",")
}

prop_seq <- bsts::GeometricSequence(5, .8, .5)

prop_seq %>% walk(function(prop){
  s_seq %>% walk(function(s) {
    ampute_sim(1e4, s, 1, prop, "high_cor", "MCAR")
    ampute_sim(1e4, s, 1, prop, "high_cor", "MAR")
    ampute_sim(1e4, s, 1, prop, "low_cor", "MAR")
  })
})

N_seq %>% walk(function(N){
  s_seq %>% walk(function(s) {
    ampute_sim(N, s, 1, .5, "high_cor", "MCAR")
    ampute_sim(N, s, 1, .5, "high_cor", "MAR")
    ampute_sim(N, s, 1, .5, "low_cor", "MAR")
  })
})



## plot correlations

# lavaan syntax for model
m <- "
  A =~ 0.7 * A1 + 0.9 * A2 + 0.9 * A3 + 0.3 * B1
  B =~ 0.7 * B1 + 0.9 * B2 + 0.9 * B3
  B ~ 0.9 * A
  "

N = 2e3
# Simulate data
d <- sim_standardized(m,
                      n = N,
                      latent = FALSE,
                      errors = FALSE)

library(GGally)
ggcorr(data = res %>% rename(T = "time_at_risk", "H" = hazard, "d" = event_status) %>%
         select(-logT) %>%
         mutate_all( ~ as.numeric(as.character(.))),
       method = c("pairwise.complete.obs", "pearson"),
       label = TRUE, label_size = 4)