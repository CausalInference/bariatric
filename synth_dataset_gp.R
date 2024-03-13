
# for grace period
synth_gp <- function(){
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  pacman::p_load(tidyverse, data.table, here, truncnorm)
  prefix <- here()
  length=10000*12
  d = data.frame(scrssn = rep(1:10000, each=12),
                 id.new=1:length,
                 interval=rep(1:12, times=10000),
                 y_surgery=rbinom(n=length, prob=0.01, size=1),
                 dyslipidemia_y_lag12=rbinom(n=length, prob=0.25, size=1),
                 hypertension_y_lag12=rbinom(n=length, prob=0.25, size=1),
                 osteoarthritis_y_lag12=rbinom(n=length, prob=0.25, size=1),
                 sleepapnea_y_lag12=rbinom(n=length, prob=0.25, size=1),
                 dyslipidemia_y_bl=rbinom(n=length, prob=0.25, size=1),
                 hypertension_y_bl=rbinom(n=length, prob=0.25, size=1),
                 osteoarthritis_y_bl=rbinom(n=length, prob=0.25, size=1),
                 sleepapnea_y_bl=rbinom(n=length, prob=0.25, size=1),
                 censored=rbinom(n=length, prob=0.05, size=1),
                 event_y=rbinom(n=length, prob=0.002, size=1),
                 eligible=rbinom(n=length, prob=0.7, size=1),
                 smoking_status=rbinom(n=length, size=2, prob=c(0.2, 0.2, 0.6)),
                 value_bmi_bl = truncnorm::rtruncnorm(n=length, a=25, b=65, mean=40, sd=4),
                 value_bmi_change_bl = rnorm(n=length),
                 value_a1c_bl = truncnorm::rtruncnorm(n=length, a=4, b=11, mean=7, sd=1),
                 value_a1c_change_bl = rnorm(n=length)
                 )
  d1 = d %>% mutate(dyslipidemia_y_lag12 = ifelse(dyslipidemia_y_lag12==0, NA, 1),
                    hypertension_y_lag12 = ifelse(hypertension_y_lag12==0, NA, 1),
                    osteoarthritis_y_lag12 = ifelse(osteoarthritis_y_lag12==0, NA, 1),
                    sleepapnea_y_lag12 = ifelse(sleepapnea_y_lag12==0, NA, 1),
                    
                    dyslipidemia_y_bl = ifelse(dyslipidemia_y_bl==0, NA, 1),
                    hypertension_y_bl = ifelse(hypertension_y_bl==0, NA, 1),
                    osteoarthritis_y_bl = ifelse(osteoarthritis_y_bl==0, NA, 1),
                    sleepapnea_y_bl = ifelse(sleepapnea_y_bl==0, NA, 1),
                    
                    y_surgery = ifelse(y_surgery==0, NA, 1),
                    
                    censored = ifelse(censored==0, NA, 1),
                    event_y = ifelse(event_y==0, NA, 1),
                    
                    pastyear_med_oralhypoglycemic_bl = rbinom(n=length, prob=0.28, size=1),
                    pastyear_med_insulin_bl = rbinom(n=length, prob=0.11, size=1)
                    ) %>% 
    group_by(scrssn) %>% 
    fill(c(dyslipidemia_y_lag12, hypertension_y_lag12, osteoarthritis_y_lag12, sleepapnea_y_lag12,
           dyslipidemia_y_bl, hypertension_y_bl, osteoarthritis_y_bl, sleepapnea_y_bl,
           y_surgery,
           censored, event_y), 
         .direction="down") %>% 
    ungroup() %>% 
    mutate(dyslipidemia_y_lag12 = ifelse(is.na(dyslipidemia_y_lag12), 0, 1),
           hypertension_y_lag12 = ifelse(is.na(hypertension_y_lag12), 0, 1),
           osteoarthritis_y_lag12 = ifelse(is.na(osteoarthritis_y_lag12), 0, 1),
           sleepapnea_y_lag12 = ifelse(is.na(sleepapnea_y_lag12), 0, 1),
           
           dyslipidemia_y_bl = ifelse(is.na(dyslipidemia_y_bl), 0, 1),
           hypertension_y_bl = ifelse(is.na(hypertension_y_bl), 0, 1),
           osteoarthritis_y_bl = ifelse(is.na(osteoarthritis_y_bl), 0, 1),
           sleepapnea_y_bl = ifelse(is.na(sleepapnea_y_bl), 0, 1),
           y_surgery = ifelse(is.na(y_surgery), 0, 1),
           
           dyslipidemia_y_bl = ifelse(dyslipidemia_y_lag12==1, 1, dyslipidemia_y_bl),
           hypertension_y_bl = ifelse(hypertension_y_lag12==1, 1, hypertension_y_bl),
           osteoarthritis_y_bl = ifelse(osteoarthritis_y_lag12==1, 1, osteoarthritis_y_bl),
           sleepapnea_y_bl = ifelse(sleepapnea_y_lag12==1, 1, sleepapnea_y_bl),
           
           censored = ifelse(is.na(censored), 0, 1),
           event_y = ifelse(is.na(event_y), 0, 1),
    )
  d2 = d1 %>% mutate(tstart.new = (interval-1)*30)
  setDT(d2)
  d2[interval==1, female := rbinom(n=sum(d2$interval==1), size=1, prob=0.1)]
  d2[interval==1, afam := rbinom(n=sum(d2$interval==1), size=1, prob=0.2)]
  d2[interval==1, age_bl := truncnorm::rtruncnorm(n=sum(d2$interval==1), a=25, b=65, mean=56, sd=2)]
  d2[interval==1, calendar.time_bl := runif(n=sum(d2$interval==1), min=730, max=5264)]
  d3 = d2 %>% group_by(scrssn) %>% 
    fill(c(female, afam, calendar.time_bl, age_bl), .direction="down") %>% 
    mutate(y_surgery_lag=ifelse(interval==1,0,lag(y_surgery))) %>% 
    ungroup() %>% 
    mutate(y_surgery_lag = ifelse(y_surgery_lag==0, NA, 1)) %>% 
    group_by(scrssn) %>% 
    fill(y_surgery_lag, .direction="down") %>% 
    ungroup() %>% 
    mutate(y_surgery_lag = ifelse(is.na(y_surgery_lag), 0, 1))
  
  ifelse(!dir.exists(file.path(prefix, "synthetic-data")),
         dir.create(file.path(prefix, "synthetic-data")), FALSE)
  ifelse(!dir.exists(file.path(prefix, "synthetic-data","bootstrap")),
         dir.create(file.path(prefix, "synthetic-data","bootstrap")), FALSE)
  data.table::fwrite(d3, file = here::here("synthetic-data","grace-period-A.txt"))
  
  num.id.new = length(unique(d3$id.new))
  dat_y = data.frame(event_y=rbinom(n=num.id.new, size=1, prob=0.1),
                     id.new=unique(d3$id.new),
                     max_int=floor(runif(n=num.id.new, min=1, max=150)))
  setDT(dat_y)
  data.table::fwrite(dat_y, file = here::here("synthetic-data","grace-period-Y.txt"))
  # 
  # smoking_grid = data.table(scrssn=1:length, date_baseline=as.Date("2005-01-01"), smoking_status=rbinom(n=length, prob=0.3, size=1))
  # ifelse(!dir.exists(file.path(prefix, "covariate-objects")),
  #        dir.create(file.path(prefix, "covariate-objects")), FALSE)
  # data.table::fwrite(smoking_grid, file=here::here("covariate-objects", "smoking-grid.csv"))
  # 
  # diabetes = data.table(scrssn=1:length, first_date_dx=as.Date("2005-01-01"))
  # data.table::fwrite(diabetes, file=here::here("covariate-objects", "diabetes.csv"))
}
synth_gp()
