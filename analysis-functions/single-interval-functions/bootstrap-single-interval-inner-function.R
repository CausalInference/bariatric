

bootstrap_single_interval_inner_function <- function(num,
                                                     mat=mat, mat_n=mat_n, dat=dat, n=n, 
                                                     bootstrap=TRUE, number.processors=number.processors,
                                                     trim=trim, stabilized=stabilized,
                                                     followup_years = 8, 
                                                     t.splines = formulas$t.splines2,
                                                     ipw.formula_n = ipw.formula_n,
                                                     ipw.formula_d = ipw.formula_d){
  # bootstrap resample by scrssn
  print(num); set.seed(num)
  if(num==0){
    dat$Freq <- 1
  } else if(num>0){
    freq.dat <- data.frame(scrssn = unique(dat$scrssn),
                           Freq = rmultinom(n=1, size=length(unique(dat$scrssn)), prob=rep(1, length(unique(dat$scrssn)))))
    setDT(freq.dat)
    dat <- freq.dat[dat, on="scrssn"]
  }
  dat <- model_predictions(dat_outcome=dat, mat=mat, stabilized=stabilized, trim=trim) # propensity scores and weights
  output_ate <- outcome_reg(dat_outcome = dat, weight_name = "w", followup_years=followup_years)
  
  output <- list("ate"=output_ate)
  
  save_name <- paste0("single-interval")
  
  ifelse(!dir.exists(file.path(prefix, "bootstrap-output", save_name)),
         dir.create(file.path(prefix, "bootstrap-output", save_name)), FALSE)
  
  save(output, file = file.path(prefix, "bootstrap-output", save_name, paste0(save_name,"-",num,".Rda")))
  print(paste0("bootstrap number completed =",num))
  return(output)
}
