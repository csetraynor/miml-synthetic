rm(list = ls())
require(mice)
require(readr)
require(purrr)
require(dplyr)
library(simstandard)
library(lavaan)

parentDir <- "DerivedData/simulations"
if(!dir.exists(parentDir)) dir.create(parentDir, recursive = T)


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
  
  parentDir <- file.path("DerivedData/simulations/float/high_cor", N, s)
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
  
  parentDir <- file.path("DerivedData/simulations/float/low_cor", N, s)
  if(!dir.exists(parentDir)) dir.create(parentDir, recursive = T)
  saveRDS(d, file.path(parentDir, "simdata.RDS"))
}

1:200 %>% walk(function(s) {
  sample_data_sim_high_cor(s, 1e4)
  sample_data_sim_low_cor(s, 1e4)
})



# library(lavaan) # only needed once per session
# model <- '
#   # measurement model
#     ind60 =~ x1 + x2 + x3
#     dem60 =~ y1 + y2 + y3 + y4
#     dem65 =~ y5 + y6 + y7 + y8
#   # regressions
#     dem60 ~ ind60
#     dem65 ~ ind60 + dem60
#   # residual correlations
#     y1 ~~ y5
#     y2 ~~ y4 + y6
#     y3 ~~ y7
#     y4 ~~ y8
#     y6 ~~ y8
# '
# fit <- sem(model, data=PoliticalDemocracy)
# summary(fit, standardized=TRUE)

### sim categorical data
# 
# library(GenOrd)
# 
# 
# # lavaan syntax for model
# m <- "
#   A =~ 0.7 * A1 + 0.9 * A2 + 0.9 * A3 + 0.3 * B1
#   B =~ 0.7 * B1 + 0.9 * B2 + 0.9 * B3
#   B ~ 0.9 * A
#   "
# R <- get_model_implied_correlations(m) 
# marginal <- list(c(0.1,0.3,0.6), c(0.4,0.7,0.9),
#                  c(0.3,0.6,0.8), c(0.2,0.3,0.5),
#                  c(0.1,0.2,0.8), c(0.2,0.4,0.6) )   
# bounds <- corrcheck(marginal)
# bounds <- bounds %>% map(function(x) as.matrix(x))
# 
# R[R < bounds[[1]] ] <- bounds[[1]][R < bounds[[1]]]
# R[R > bounds[[2]] ] <- bounds[[2]][R > bounds[[2]]]
# 
# d <- ordsample(5000, marginal, R, cormat = "continuous")
# cov(d)
# 
# 
# 
# 
# 
# 
# 
# 
# ###################################################################
# library(lavaan)
# 
# model <- "mpg ~ lambda1*cyl + lambda2*disp + drat + disp:drat
#           disp ~ cyl
#           interac := lambda1*lambda2" 
# fit <- sem(model, data = mtcars) 
# summary(fit)
# 
# d <- sim_standardized(model,
#                       n = N,
#                       latent = FALSE,
#                       errors = FALSE)
# m_free <- fixed2free(model)
# lav_results <- sem(
#   model = model, 
#   data = as_tibble(d) )
# res1 <- lavaan::parameterEstimates(lav_results,  standardized = TRUE)
# 
# 
# 
# # create a new variable with the interaction of L1 and L2
# semdata <- semdata %>%
#   mutate(L1L2 = L1 * L2)
# 
# 
# require(dplyr)
# library(lavaan)
# # Make model m free
# # lavaan syntax for model
# m <- "
#   A =~ 0.7 * A1 + 0.9 * A2 + 0.9 * A3 + 0.3 * B1
#   B =~ 0.7 * B1 + 0.9 * B2 + 0.9 * B3
#   B ~ 0.9 * A
#   "
# # Simulate data
# d <- sim_standardized(m,
#                       n = N,
#                       latent = FALSE,
#                       errors = FALSE)
# 
# m_free <- fixed2free(m)
# lav_results <- sem(
#   model = m_free, 
#   data = as_tibble(d) )
# res1 <- lavaan::parameterEstimates(lav_results,  standardized = TRUE)
# 
# 
# fitcfa <- cfa(data = d, model = m)
# 
# # 2. extract the predicted values of the cfa and add them to the dataframe
# semdata <- data.frame(as.data.frame(d), predict(fitcfa))
# 
# # create a new variable with the interaction of L1 and L2
# semdata <- semdata %>%
#   mutate(L1L2 = L1 * L2)
# 
# # 3. now set up the regression and add the predefined interaction to the model
# # a) regression with both latent variables and the interaction
# semmodel1 <- "
#     # define regression
#     mpg ~ L1 + L2 + L1L2
# "
# 
# # lavaan syntax for model
# m2 <- "
#   A =~ 0.7 * A1 + 0.9 * A2 + 0.9 * A3 * A2 + 0.9 * A3 + 0.3 * B1
#   B =~ 0.7 * B1 + 0.9 * B2 + 0.9 * B3
#   B ~ 0.9 * A 
#   "
# # Simulate data
# d2 <- sim_standardized(m2,
#                       n = N,
#                       latent = FALSE,
#                       errors = FALSE)
# 
# m_free2 <- fixed2free(m2)
# lav_results2 <- sem(
#   model = m_free2, 
#   data = as_tibble(d2) )
# res2 <- lavaan::parameterEstimates(lav_results2,  standardized = TRUE)
# 
# 
# 
# 
# 
# lav_results <- sem(
#   model = m_free, 
#   data = as_tibble(d) %>% mutate_all(as.ordered) )
# my_parameter_estimates <- function(object, header       = TRUE,
#                                    fit.measures = FALSE,
#                                    estimates    = TRUE,
#                                    ci           = FALSE,
#                                    fmi          = FALSE,
#                                    std          = FALSE,
#                                    standardized = FALSE,
#                                    cov.std      = TRUE,
#                                    rsquare      = FALSE,
#                                    std.nox      = FALSE,
#                                    modindices   = FALSE,
#                                    nd = 3L) {
#   lavaan::parameterestimates(object,  standardized = standardized)
# }
# lavaan::parameterEstimates(lav_results,  standardized = TRUE)
# 
# # Make model m free
# m_free <- fixed2free(m)
# # Display model m_free
# cat(m_free)
# 
# mar_high_correlation <- function(boot) {
#   # lavaan syntax for model
#   m <- "
#   A =~ 0.7 * A1 + 0.9 * A2 + 0.9 * A3 + 0.3 * B1
#   B =~ 0.7 * B1 + 0.9 * B2 + 0.9 * B3
#   B ~ 0.9 * A
#   "
#   
#   # Simulate data
#   d <- sim_standardized(m,
#                         n = 100000,
#                         latent = FALSE,
#                         errors = FALSE)
#   
#   resPath <- file.path(parentDir, boot)
#   write_rds(d, file.path())
# }
# 
# 
# # Evaluate the fit of model m_free on data d
# library(lavaan)
# lav_results <- sem(
#   model = m_free, 
#   data = d)
# 
# # Display summary of model
# res <- summary(
#   lav_results, 
#   standardized = TRUE, 
#   fit.measures = TRUE,
#   ci = TRUE)
# 
# # Display First 6 rows
# head(d) 
# 
# cov(d) %>% 
#   ggcor
# 
# 
# # lavaan syntax for model
# m <- "
# A =~ 0.1 * A1 + 0.1 * A2 + 0.1 * A3 + 0.1 * B1
# B =~ 0.1 * B1 + 0.1 * B2 + 0.1 * B3
# B ~ 0.1 * A
# "
# 
# # Simulate data
# d <- sim_standardized(m,
#                       n = 100000,
#                       latent = FALSE,
#                       errors = FALSE)
# # Display First 6 rows
# head(d) 
# 
# cov(d) %>% 
#   ggcor
# 
# 
# ggcor <- function(d) {
#   require(ggplot2)
#   as.data.frame(d) %>%
#     tibble::rownames_to_column("rowname") %>%
#     tidyr::pivot_longer(-rowname, names_to = "colname", values_to = "r") %>%
#     dplyr::mutate(rowname = forcats::fct_inorder(rowname) %>% forcats::fct_rev()) %>% 
#     dplyr::mutate(colname = factor(colname, 
#                                    levels = rev(levels(rowname)))) %>% 
#     ggplot(aes(colname, rowname, fill = r)) +
#     geom_tile(color = "gray90") +
#     geom_text(aes(label = formatC(r, digits = 2, format = "f") %>% 
#                     stringr::str_replace_all("0\\.",".") %>% 
#                     stringr::str_replace_all("1.00","1")), 
#               color = "white", 
#               fontface = "bold",
#               family = "serif") +
#     scale_fill_gradient2(NULL,
#                          na.value = "gray20",
#                          limits = c(-1.01, 1.01),
#                          high = "#924552",
#                          low = "#293999") +
#     coord_equal() +
#     scale_x_discrete(NULL,position = "top") +
#     scale_y_discrete(NULL) +
#     theme_light(base_family = "serif", base_size = 14) 
# }
# 
# cov(d) %>% 
#   ggcor
# 
