rm(list = ls())

.libPaths(c("ProjectLibrary/3.5",.libPaths()))
require(dplyr)
require(simtte)
require(mice)
require(readr)
require(purrr)
library(simstandard)
library(lavaan)
library(survival)
require(miceadds)
require(purrr)
require(ggplot2)

cor_level <- c("high_cor", "low_cor")
N <- 1e4
S <- 1:25

set.seed(123456)
simdata <- list( high_cor = read_rds("DerivedData/simulations/numeric/simttedata.RDS"), 
                 low_cor = read_rds("DerivedData/simulations/numeric/simttedata_lowcor.RDS") ) 

## plot correlation
high_cor <- simdata$high_cor[[25]]$res %>% bind_cols(simdata$high_cor[[25]]$simdata)
low_cor <- simdata$low_cor[[25]]$res %>% bind_cols(simdata$low_cor[[25]]$simdata)

library(GGally)
p1 <- ggcorr(data = high_cor %>% select(-ID, -lp) %>% mutate(H = mice::nelsonaalen(as.data.frame(.), sim_time, sim_status)) %>%
         rename(T = sim_time, d = sim_status) %>% select(A1:B3, H, T, d),
       method = c("pairwise.complete.obs", "pearson"),
       label = TRUE, label_size = 4)

p2 <- ggcorr(data = low_cor %>% select(-ID, -lp) %>% mutate(H = mice::nelsonaalen(as.data.frame(.), sim_time, sim_status)) %>%
         rename(T = sim_time, d = sim_status) %>% select(A1:B3, H, T, d),
       method = c("pairwise.complete.obs", "pearson"),
       label = TRUE, label_size = 4)



ggsave(plot = cowplot::plot_grid(p1, p2, labels = c("I", "II")), filename = "~/carlos-mitabnet-stats-journal/Img/sim_correlation.pdf", device = cairo_pdf)
ggsave(plot = cowplot::plot_grid(p1, p2, labels = c("I", "II")), filename = "~/carlos-mitabnet-stats-journal/Img/sim_correlation.pdf", device = cairo_pdf)


oriRes <- cor_level %>% map(function(setting) {
  simdata[[setting]] %>% map(function(x){
  coxph(Surv(sim_time, sim_status) ~ . , 
        data = x$res %>% select(sim_time, sim_status) %>%
          bind_cols(x$simdata)) %>%
    tidy()
})
}) %>%
  set_names(cor_level)

compute_mifit <- function(parentDir, resDir, mod, mech, boot, oriDat_y) {
  print(mod); print(boot)
  # if(mod != "cca") {
  #   foo <- file.path(resDir, "10000", boot, "0.5/1", mech, mod, "imp.RDS")
  #   res <- readr::read_rds(foo)
  # } else {
  #   foo <- file.path(parentDir, "10000", boot, "0.5/1", mech, "amputed_data.csv")
  #   res <- read_csv(foo)
  # }
  
  if(mod %in% c("em", "glm", "pca", "rf", "tabnet")) {
    foo <- file.path(resDir, "10000", boot, "0.5/1", mech, mod, "imp.RDS")
    res <- readr::read_rds(foo)
    
    if(mod == "em") {
      res <- miceadds::datlist2mids( res$imputations )
    }
    
    
  } else if(mod == "cca") {
    
    foo <- file.path(parentDir, "10000", boot, "0.5/1", mech, "amputed_data.csv")
    res <- vroom::vroom(delim =",", foo)
    
  } else if(mod == "GAIN-Yoon"){
    
    foo <- file.path(resDir, "10000", boot, "0.5/1", mech, mod)
    res <- 1:5 %>% map(function(M){
      out <- read_csv(file.path(foo, M - 1, "pred.csv"))
      colnames(out) <- colnames(oriDat_y)[3:ncol(oriDat_y)]
      out
    })
    res <- miceadds::datlist2mids( res )
    
  } else if(mod == "vae"){
    
    foo <- file.path("imputation-dgm/models/mrna/simulations", mech, "10000", boot, "0.5/1", mod)
    res <- 1:5 %>% map(function(M){
      out <- read_csv(file.path(foo, M - 1, "res.csv"))
      colnames(out) <- colnames(oriDat_y)[3:ncol(oriDat_y)]
      out
    })
    res <- miceadds::datlist2mids( res )
  }

  oriDat_x <- read_rds( file.path(parentDir, "10000", boot, "0.5/1", mech, "amputed_obj.RDS") )
  oriDat_x <- oriDat_x$data %>% as_tibble()
  
  oriDat <- oriDat_x %>% left_join(oriDat_y)
  
  intCoeffs <- oriDat_y %>% dplyr::select(-OS_MONTHS, -OS_STATUS) %>% colnames()
  survform <- paste0("Surv(OS_MONTHS, OS_STATUS) ~ ", paste(intCoeffs, collapse = " + "))
  OS_MONTHS <- oriDat_y %>% dplyr::pull(OS_MONTHS)
  OS_STATUS <- oriDat_y %>% dplyr::pull(OS_STATUS)
  
  if(mod != "cca") {
    fit <- with(data = res, exp = coxph(as.formula(survform)))
    fit <- pool(fit)
    out <- compute_stats(oriDat, fit) 
  } else {
    fit <- with(data = res, exp = coxph(as.formula(survform)))
    fit <- tidy(fit)
    out <- compute_stats_cca(oriDat, fit) 
  }
  
  return(out)
}

compute_stats_cca <- function(oriDat, fit) {
  intCoeffs <- oriDat %>% dplyr::select(-OS_MONTHS, -OS_STATUS) %>% colnames()
  survform <- paste0("Surv(OS_MONTHS, OS_STATUS) ~ ", paste(intCoeffs, collapse = " + "))
  oriCoeffs <- coxph(as.formula(survform), oriDat) %>% coef()
  # compCoeffs <- coxph(as.formula(survform) , misDat) %>% coef()
  bias <- abs(oriCoeffs - fit$estimate)
  se <- fit$std.error
  estimate <- fit$estimate
  ub <- estimate + 1.96 * se
  lb <- estimate - 1.96 * se
  iw <- ub - lb
  cover <- (oriCoeffs >= lb) & (oriCoeffs <= ub)
  tibble(par = names(cover) , bias, se, iw, cover, ub, lb, estimate)
}

compute_stats <- function(oriDat, fit) {
  intCoeffs <- oriDat %>% dplyr::select(-OS_MONTHS, -OS_STATUS) %>% colnames()
  survform <- paste0("Surv(OS_MONTHS, OS_STATUS) ~ ", paste(intCoeffs, collapse = " + "))
  oriCoeffs <- coxph(as.formula(survform), oriDat) %>% coef()
  # compCoeffs <- coxph(as.formula(survform) , misDat) %>% coef()
  bias <- abs(oriCoeffs - fit$pooled$estimate)
  se <- sqrt(fit$pooled$t)
  estimate <- fit$pooled$estimate
  ub <- fit$pooled$estimate + 1.96 * se
  lb <- fit$pooled$estimate - 1.96 * se
  iw <- ub - lb
  cover <- (oriCoeffs >= lb) & (oriCoeffs <= ub)
  tibble(par = names(cover) , bias, se, iw, cover, ub, lb, estimate)
}

resDir <- "Results/simulations/numeric"
parentDir <- "DerivedData/simulations/numeric"
mechs <- c("MAR")
boots <- seq(1, 25, by = 1)
models <- c("cca","GAIN-Yoon", "em", "glm", "pca", "rf", "tabnet")
cor_level <- c( "low_cor" , "high_cor"  )


set.seed(123456)
require(furrr)
plan(multisession, workers = 2)

resMI <- cor_level %>% map(function(cor_setting) {
  models %>% map(function(mod){
  boots %>% map(function(boot){
    mechs %>% map(function(mech){
      resDir_i <- file.path(resDir, cor_setting)
      parentDir_i <- file.path(parentDir, cor_setting)
      oriDat_y <- simdata[[cor_setting]][[boot]]$res %>% 
        dplyr::select(OS_MONTHS = sim_time, OS_STATUS = sim_status) %>%
        bind_cols(simdata[[cor_setting]][[boot]]$simdata)
      compute_mifit(parentDir_i, resDir_i, mod, mech, boot, oriDat_y)
    }) %>%
      set_names(mechs) %>%
      bind_rows(.id = "mech")
  }) %>%
    set_names(boots) %>%
    bind_rows(.id = "boots")
}) %>%
  set_names(models) %>%
  bind_rows(.id = "model")
}) %>%
  set_names(cor_level) %>%
  bind_rows(.id = "cor_level")

saveRDS(resMI, "Results/resmi_scm.RDS")


#### 
sumresMI <- resMI %>%
  group_by(par, model, mech) %>%
  summarise(          
    bias_sd = sd(log(bias)),
    iw_sd = sd(iw),
    ub_mu   = mean(ub),
    lb_mu   = mean(lb),
    estimate_mu = mean(estimate),
    bias_mu = mean(log(bias)),
    se_mu   = mean(se),
    iw_mu   = mean(iw),
    cover = sum(cover) / n()
  )

oriPlotDat <- seq_along(boots) %>% map(function(boot){
  oriDat_y <- simdata[[boot]]$res %>% 
    dplyr::select(OS_MONTHS = sim_time, OS_STATUS = sim_status) %>%
    bind_cols(simdata[[boot]]$simdata)
  intCoeffs <- oriDat_y %>% dplyr::select(-OS_MONTHS, -OS_STATUS) %>% colnames()
  survform <- paste0("Surv(OS_MONTHS, OS_STATUS) ~ ", paste(intCoeffs, collapse = " + "))
  oriCoeffs <- coxph(as.formula(survform), oriDat_y) 
  
  oriPlotDat <- oriCoeffs %>% tidy()
  oriPlotDat <- oriPlotDat %>%
    mutate(ub = estimate + 1.96*std.error,
           lb = estimate - 1.96*std.error)
  oriPlotDat 
})

saveRDS(oriPlotDat, "Results/oriParameters.RDS")

restab <- sumresMI %>% rename(term = par)
ggplot(restab %>%
         mutate(ub = bias_mu + 1.96*bias_sd,
                lb = bias_mu - 1.96*bias_sd,
         )
       , aes(x = model,
             y = bias_mu)) +
  geom_point() +
  geom_pointrange(aes(ymin = lb, ymax = ub)) +
  facet_grid(mech ~ term, scales = "free_y") +
  # geom_hline(data = oriPlotDat,
  #            aes(yintercept = estimate), linetype = 2)  +
  # geom_hline(data = oriPlotDat,
  #            aes(yintercept = ub), linetype = 2)  +
  # geom_hline(data = oriPlotDat,
  #            aes(yintercept = lb), linetype = 2) +
  labs(y = expression(Bias ~ theta), x = "Model") +
  theme_bw() +
  ggpubr::labs_pubr() +
  theme(axis.text.x = element_text(angle = 90))

