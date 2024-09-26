# wrapper single interval
bootstrap_single_interval_function <- function(followup_years,
                                               seed_vector, number.processors,
                                               censor_surgery=TRUE,
                                               trim, stabilized, 
                                               ipw.formula_n = NULL,
                                               ipw.formula_d,
                                               ...){
  full_data_preamble()
  
  source(file.path(prefix, "analysis-functions/model_formulas.R"), echo=FALSE)
  sapply(c("single-interval-restriction-function.R", "model-predictions-function.R", 
           "moma-function.R", "prep-function.R", "outcome-regression-function.R",
           "bootstrap-single-interval-inner-function.R"),
         function(x) {source(file.path(prefix, "analysis-functions", "single-interval-functions", x), echo=FALSE)}
  )
  truncate_years <- followup_years + 2
  vars_a <- c("id.new", "scrssn", "censored", "event_y", "y_surgery", "eligible", "tstart.new",
              "value_bmi", "value_bmi_bl", "value_bmi_change_bl", "date_baseline", "theyear",
              "diabetes_y_bl", "value_a1c_bl",
              all.vars(as.formula(ipw.formula_n)), all.vars(as.formula(ipw.formula_d))) %>% unique %>% 
    {.[. != "followup_years"]}
  
  dat <- data.table::fread(file = paste0(prefix,"/synthetic-data/","single-interval.csv"),
                           select = vars_a,
                           nrows = Inf) %>% {.[complete.cases(.),]}
  dat <- data_prep_function(dat=dat, truncate_years=truncate_years)
  argg <- as.list(environment())
  
  print("creating model matrix")
  mat <- moma(dat=dat, ipw.formula_d=ipw.formula_d) # model matrix
  mat_n <- NULL
  
  if(censor_surgery){
    # only required if using synthetic dataset: creates *synthetic* date of surgery for 10% of individuals who were "assigned" not to undergo surgery - this should NOT be done for a true analysis (this is only required because of the synthetic dataset)
    surgery_vector <- sample(dat$id.new[dat$y_surgery==0], size = sum(dat$y_surgery==0)*0.1)
    dat$tstart.new2 <- NA
    dat$tstart.new2[surgery_vector] <- dat$tstart.new[surgery_vector]*(runif(length(surgery_vector), min = 0, max = 1))
    dat$event_y[!is.na(dat$tstart.new2)] <- 0
    dat$tstart.new[!is.na(dat$tstart.new2)] <- dat$tstart.new2[!is.na(dat$tstart.new2)]
    dat$tstart.new2 <- NULL
  }
  
  cols_delete <- setdiff(names(dat), c("id.new", "scrssn", "eligible", "y_surgery", "event_y", "tstart.new", "Freq"))
  dat[, (cols_delete) := NULL] # reduce size of dataset
  
  bs_results <- pbapply::pblapply(seed_vector, 
                                  function(z) {
                                    output <- bootstrap_single_interval_inner_function(
                                      num=z,
                                      mat=mat, mat_n=mat_n, n=n, dat=dat,
                                      bootstrap=TRUE, number.processors=number.processors,
                                      trim=trim, stabilized=stabilized,
                                      followup_years = followup_years, 
                                      t.splines = formulas$t.splines2,
                                      ipw.formula_n = ipw.formula_n,
                                      ipw.formula_d = ipw.formula_d,
                                      ...)
                                  })
  return(bs_results)
}
