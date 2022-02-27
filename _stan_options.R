# Stan options 

suppressPackageStartupMessages({
  library("rstan")
  library("cmdstanr")
  library("posterior")
  library("loo")
})

rstan_options(auto_write = TRUE) # avoid recompilation of models
options(mc.cores = parallel::detectCores()) # parallelize across all CPUs
Sys.setenv(LOCAL_CPPFLAGS = "-march=native") # improve execution time
