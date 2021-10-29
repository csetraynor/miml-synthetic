rm(list = ls())

.libPaths(c("./ProjectLibrary/3.5",
            "/opt/scp/software/mro/3.5.1-foss-2017a/lib64/R/site-library",
            "/opt/scp/software/mro/3.5.1-foss-2017a/lib64/R/library") )

library(readr)
library(dplyr)
library(drake)
library(future.batchtools)
future::plan(batchtools_slurm, template = "../SLURM/batchtools_gpu_slurm.tmpl")

tabnet_impute_float <- function(cor_level, N, s, prop, boot, mech, M = 20) {
  
  foo <- file.path("DerivedData/simulations/float", cor_level, N, s, prop, boot, mech)
  mis_data <- read_csv(file.path( foo, "amputed_data.csv") )
  
  ## load dev mice
  devtools::document("mice")
  
  imp <- mice(mis_data, m = M, maxit = 5, method = "tabnetRegressor", gpu = T, hpc = F )
  
  resPath <- file.path("Results/simulations/float", cor_level, N, s, prop, boot, mech, "tabnet")
  
  if(!dir.exists(resPath)) dir.create(resPath, recursive = TRUE)
  saveRDS(imp, file.path(resPath, "imp.RDS") )
  
  return(0)
}


run_plan <- drake_plan (
  imp_mar = target(
    tabnet_impute_float(cor_level, N, s, prop, boot, mech, M = 20),
    transform = cross(cor_level = !!c("high_cor", "low_cor"),
                    N = !!c(1e3),
                    s = !!c(1:200),
                    prop = !!c(0.5),
                    boot = !!c(1),
                    mech = !!("MAR")
                    ),
    hpc = TRUE,
    resources = list(ncpus = 2, walltime = 48*60, memory = 8000)
  ),
  imp_mcar = target(
    tabnet_impute_float(cor_level, N, s, prop, boot, mech, M = 20),
    transform = cross(cor_level = !!c("high_cor"),
                      N = !!c(1e3),
                      s = !!c(1:200),
                      prop = !!c(0.5),
                      boot = !!c(1),
                      mech = !!("MCAR")
    ),
    hpc = TRUE,
    resources = list(ncpus = 2, walltime = 48*60, memory = 8000)
  )
)

make(run_plan,
     verbose = 3,
     jobs = nrow(run_plan),
     parallelism = "future",
     lock_envir = FALSE,
     log_progress = TRUE,
     lazy_load = "promise",
     console_log_file = "slurm_run.out",
     memory_strategy = "lookahead",
     caching = "worker",
     keep_going = FALSE )

