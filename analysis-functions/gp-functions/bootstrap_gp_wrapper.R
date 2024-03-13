# bootstrap gp wrapper
bootstrap_gp_wrapper_function <- function(bootstrap=TRUE, seed_vector, save_name=NULL, 
                                          name_dat, number.processors, censor_surgery,
                                          followup_years, grace.intervals, min.interval,
                                          trim, ipw.formula_n=NULL, ipw.formula_d, ...){
  lapply(c("tidyverse", "reshape2", "survival", "splines", "lubridate", "speedglm", "data.table"),
         function(x) suppressPackageStartupMessages(library(x, character.only=TRUE, quietly=TRUE)))
  full_data_preamble()
  
  source(paste0(prefix,"/analysis-functions/gp-functions/gp_weights_function.R"))
  source(paste0(prefix,"/analysis-functions/gp-functions/gp_outcome_models_function.R"))
  source(paste0(prefix,"/analysis-functions/gp-functions/bootstrap_gp_inner.R"))
  source(paste0(prefix,"/analysis-functions/tt-plot_function.R"))
  source(paste0(prefix,"/analysis-functions/strip-glm_function.R"))
  source(paste0(prefix,"/analysis-functions/numerator_function.R"))
  source(paste0(prefix,"/analysis-functions/model_formulas.R"))
  source(paste0(prefix,"/analysis-functions/blb-outcome-weights_function.R"))
  
  vars_a <- c("id.new", "scrssn", "censored", "event_y", "y_surgery", "y_surgery_lag", "interval", "eligible", "tstart.new", 
              all.vars(as.formula(ipw.formula_d))) %>% unique %>% {.[. != "followup_years"]}
  dat_a <- data.table::fread(file = paste0(prefix,"/synthetic-data/grace-period-A",".txt"), 
                             select=vars_a)
  dat_y <- fread(file = paste0(prefix,"/synthetic-data/","grace-period-Y.txt"))[id.new %in% unique(dat_a$id.new)] # loads overall population, then restricted by dat_a
  
  # save multinom
  unique.ssns <- unique(dat_a$scrssn)
  N <- length(unique.ssns)
  set.seed(1)
  scrssn_sampling  <- rmultinom(n=length(seed_vector), 
                                size=N, 
                                prob=rep(1, N)) %>% as.data.table
  scrssn_sampling[, scrssn := unique(dat_a$scrssn)]
  fwrite(scrssn_sampling, file=paste0(prefix, "/synthetic-data/bootstrap/resample-weights-gp.csv"))
  
  mat <- model.matrix(as.formula(ipw.formula_d), dat=dat_a)
  dat_a <- dat_a[,.(scrssn, id.new, interval, y_surgery, y_surgery_lag, eligible, event_y, tstart.new)]
  
  bs_results <- parallel::mclapply(c(0, seed_vector), 
                                   function(x) {
                                     setDTthreads(1)
                                     output <- bootstrap_gp_inner_function(mat=mat, mat_n=NULL, dat_a=dat_a, dat_y_new=dat_y, num=x,
                                                                     bootstrap=TRUE, number.processors=number.processors,
                                                                     name_dat="diabetics-cv", min.interval=min.interval,
                                                                     trim=trim, censor_surgery,
                                                                     followup_years = followup_years, grace.intervals=grace.intervals,
                                                                     t.splines = formulas$t.splines2,
                                                                     ipw.formula_n = NULL,
                                                                     ipw.formula_d = ipw.formula_d, ...)
                                   }, 
                                   mc.cores=number.processors)
  return(bs_results)
}
