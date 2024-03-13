
# for single interval
synth <- function(){
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  pacman::p_load(tidyverse, data.table, truncnorm, here)
  prefix <- here()
  
  length = 2000000
  d = data.frame(censored = rbinom(n=length, prob=0.006, size=1),
             event_y=rbinom(n=length, prob=0.16, size=1),
             y_surgery=rbinom(n=length, prob=0.002, size=1),
             calendar.time_bl=truncnorm::rtruncnorm(n=length, a=730, b=5264, mean=3100, sd=1000),
             value_bmi_bl = truncnorm::rtruncnorm(n=length, a=25, b=65, mean=40, sd=4),
             value_bmi_change_bl = rnorm(n=length),
             theyear=floor(runif(length, 2007, 2019)),
             value_a1c_bl = truncnorm::rtruncnorm(n=length, a=4, b=11, mean=7, sd=1),
             female=rbinom(n=length, prob=0.11, size=1),
             afam=rbinom(n=length, prob=0.23, size=1),
             age_bl = truncnorm::rtruncnorm(n=length, a=25, b=65, mean=56, sd=2),
             dyslipidemia_y_lag12 = rbinom(n=length, prob=0.80, size=1),
             hypertension_y_lag12 = rbinom(n=length, prob=0.83, size=1),
             osteoarthritis_y_lag12 = rbinom(n=length, prob=0.38, size=1),
             sleepapnea_y_lag12 = rbinom(n=length, prob=0.22, size=1),
             value_a1c_change_bl = rnorm(n=length),
             pastyear_med_oralhypoglycemic_bl = rbinom(n=length, prob=0.28, size=1),
             pastyear_med_insulin_bl = rbinom(n=length, prob=0.11, size=1),
             eligible=1,
             diabetes_y_bl=1,
             id.new=1:length,
             scrssn=1:length,
             tstart.new=floor(runif(length, 0, 4470)),
             date_baseline=as.Date("2005-01-01")
             )
  d2 = d %>% mutate(value_bmi=value_bmi_bl,
                    dyslipidemia_y_bl = ifelse(dyslipidemia_y_lag12==1,1,rbinom(n=length, prob=0.80, size=1)),
                    hypertension_y_bl = ifelse(hypertension_y_lag12==1,1,rbinom(n=length, prob=0.83, size=1)),
                    osteoarthritis_y_bl = ifelse(osteoarthritis_y_lag12==1,1,rbinom(n=length, prob=0.38, size=1)),
                    sleepapnea_y_bl = ifelse(sleepapnea_y_lag12==1,1,rbinom(n=length, prob=0.22, size=1)))
  setDT(d2)
  ifelse(!dir.exists(file.path(prefix, "synthetic-data")),
         dir.create(file.path(prefix, "synthetic-data")), FALSE)
  data.table::fwrite(d2, file=here::here("synthetic-data","single-interval.csv"))
}
synth()
