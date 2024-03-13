bootstrap_gp_inner_function <- function(bootstrap,
                                        mat, mat_n, 
                                        dat_a, dat_y_new, num, number.processors,
                                        name_dat, min.interval, censor_surgery,
                                        followup_years, 
                                        t.splines, grace.intervals,
                                        trim, ipw.formula_n=NULL, ipw.formula_d, ...){
  lapply(c("tidyverse", "reshape2", "survival", "splines", "lubridate", "speedglm", "data.table"),
         function(x) suppressPackageStartupMessages(library(x, character.only=TRUE, quietly=TRUE)))
  full_data_preamble()
  
  source(paste0(prefix,"/analysis-functions/strip-glm_function.R"))
  source(paste0(prefix,"/analysis-functions/numerator_function.R"))
  source(paste0(prefix,"/analysis-functions/model_formulas.R"))
  source(paste0(prefix,"/analysis-functions/blb-outcome-weights_function.R"))
  source(paste0(prefix,"/analysis-functions/gp-functions/gp_weights_function.R"))
  source(paste0(prefix,"/analysis-functions/gp-functions/gp_outcome_models_function.R"))
  
  if(num==0){
    resamples <- fread(file=paste0(prefix, "/synthetic-data/bootstrap/resample-weights-gp.csv"), 
                       select=c("scrssn", paste0("V", 1)))
    setnames(resamples, c("scrssn", "Freq"))
    resamples$Freq <- 1
  } else if(num>0){
    resamples <- fread(file=paste0(prefix, "/synthetic-data/bootstrap/resample-weights-gp.csv"), 
                       select=c("scrssn", paste0("V", num)))
    setnames(resamples, c("scrssn", "Freq"))
  }
  dat_a_weights <- resamples[dat_a[,.(scrssn)], on="scrssn"]
  print("fitting treatment model...")
  model_subset_index <- dat_a$eligible==1 & dat_a$y_surgery_lag==0 & dat_a$interval >= min.interval
  t_d <- speedglm::speedglm.wfit(y=dat_a$y_surgery[model_subset_index],
                                 X=mat[model_subset_index,],
                                 weights=dat_a_weights$Freq[model_subset_index],
                                 family=quasibinomial())
  dat_a$t_d_preds <- plogis(mat %*% coef(t_d)); rm(mat); gc()
  
  dat_a$pred_t_n_vector <- 1
  
  weights_list <- gp_weights_function(dat_a=dat_a, min.interval=min.interval, grace.intervals=grace.intervals, trim=trim)
  print("row weights done")
  
  dat_y_m <- blb_outcome_weights(parallel=FALSE, bootstrap=bootstrap, censor_surgery=censor_surgery,
                                 w_1_prod=weights_list$w_1_prod_trim_vector,
                                 w_0_prod=weights_list$w_0_prod_trim_vector,
                                 dat_a=dat_a, dat_a_weights=dat_a_weights, dat_y_new=dat_y_new,
                                 max.interval=(followup_years)*12, grace_period_duration = grace.intervals)
  
  output <- gp_outcome_models_function(dat_outcome=dat_y_m, t.splines=t.splines, followup_years)
  
  ifelse(!dir.exists(file.path(prefix, "bootstrap-output", "grace-period")),
         dir.create(file.path(prefix, "bootstrap-output", "grace-period")), FALSE)
  
  save(output, file=paste0(prefix,"/bootstrap-output/grace-period/","grace-period-",num,".Rda"))
  return(output)
}
