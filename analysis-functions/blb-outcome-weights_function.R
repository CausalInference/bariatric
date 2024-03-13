blb_outcome_weights <- function(parallel, bootstrap=TRUE, censor_surgery,
                                w_1_prod, w_0_prod, grace_period_duration,
                                dat_a, dat_a_weights, max.interval, dat_y_new){
  print("starting outcome weights"); print(Sys.time())
  
  # data.table for weights
  if(is.null(dat_a_weights)){
    dat_a_grace <- data.table(w_1_prod_trim=w_1_prod[dat_a$interval==grace_period_duration],
                              w_0_prod_trim=w_0_prod[dat_a$interval==grace_period_duration],
                              Freq=rep(1, sum(dat_a$interval==grace_period_duration)),
                              id.new=dat_a$id.new[dat_a$interval==grace_period_duration])
    
    grace_w_1 <- aggregate(w_1_prod, sum, by=list(dat_a$event_y, dat_a$tstart.new))
    grace_w_0 <- aggregate(w_0_prod, sum, by=list(dat_a$event_y, dat_a$tstart.new))
  } else if (!is.null(dat_a_weights)){
    dat_a_grace <- data.table(w_1_prod_trim=w_1_prod[dat_a$interval==grace_period_duration],
                              w_0_prod_trim=w_0_prod[dat_a$interval==grace_period_duration],
                              Freq=dat_a_weights$Freq[dat_a$interval==grace_period_duration],
                              id.new=dat_a$id.new[dat_a$interval==grace_period_duration])
    
    grace_w_1 <- aggregate(w_1_prod*dat_a_weights$Freq, sum, by=list(dat_a$event_y, dat_a$tstart.new))
    grace_w_0 <- aggregate(w_0_prod*dat_a_weights$Freq, sum, by=list(dat_a$event_y, dat_a$tstart.new))
  }
  # first 6 months for a = 1
  d_11.first <- grace_w_1 %>% filter(Group.1==1) %>% .$x # event_y==1
  if(length(d_11.first)<grace_period_duration){
    num.zeros <- grace_period_duration - length(d_11.first)
    d_11.first <- c(rep(0,num.zeros), d_11.first)
  }
  
  d_10.first <- grace_w_1 %>% filter(Group.1==0) %>% .$x # event_y==0
  if(length(d_10.first)<grace_period_duration){
    num.zeros <- grace_period_duration - length(d_10.first)
    d_10.first <- c(rep(0,num.zeros), d_10.first)
  }
  
  # first 6 months for a = 0
  d_01.first <- grace_w_0 %>% filter(Group.1==1) %>% .$x
  if(length(d_01.first)<grace_period_duration){
    num.zeros <- grace_period_duration - length(d_01.first)
    d_01.first <- c(rep(0,num.zeros), d_01.first)
  }
  
  d_00.first <- grace_w_0 %>% filter(Group.1==0) %>% .$x
  if(length(d_00.first)<grace_period_duration){
    num.zeros <- grace_period_duration - length(d_00.first)
    d_00.first <- c(rep(0,num.zeros), d_00.first)
  }
  
  if(censor_surgery){
    # creates *synthetic* date of surgery for 10% of individuals who were "assigned" not to undergo surgery
    dat <- dat_a
    dat_y_new_0 <- dat_y_new
    surgery_vector <- sample(dat_a$id.new[dat$y_surgery==0], size = sum(dat$y_surgery==0)*0.1)
    dat_y_new_0$max_int2 <- NA
    dat_y_new_0$max_int2[surgery_vector] <- dat_y_new_0$max_int[surgery_vector]*(runif(length(surgery_vector), min = 0, max = 1))
    dat_y_new_0$event_y[!is.na(dat_y_new_0$max_int2)] <- 0
    dat_y_new_0$max_int[!is.na(dat_y_new_0$max_int2)] <- dat_y_new_0$max_int2[!is.na(dat_y_new_0$max_int2)]
    dat_y_new_0$max_int2 <- NULL
    
    compute_outcome_weights <- function(a_event){
      if(a_event[1]==1 & a_event[2]==1){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new$id.new[dat_y_new$event_y==1 & dat_y_new$max_int==i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_1_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      } else if (a_event[1]==1 & a_event[2]==0){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new$id.new[dat_y_new$event_y==0 & dat_y_new$max_int>=i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_1_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      } else if(a_event[1]==0 & a_event[2]==1){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new_0$id.new[dat_y_new_0$event_y==1 & dat_y_new_0$max_int==i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_0_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      } else if (a_event[1]==0 & a_event[2]==0){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new_0$id.new[dat_y_new_0$event_y==0 & dat_y_new_0$max_int>=i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_0_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      }
    }
  } else if(!censor_surgery){
    compute_outcome_weights <- function(a_event){
      if(a_event[1]==1 & a_event[2]==1){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new$id.new[dat_y_new$event_y==1 & dat_y_new$max_int==i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_1_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      } else if (a_event[1]==1 & a_event[2]==0){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new$id.new[dat_y_new$event_y==0 & dat_y_new$max_int>=i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_1_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      } else if(a_event[1]==0 & a_event[2]==1){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new$id.new[dat_y_new$event_y==1 & dat_y_new$max_int==i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_0_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      } else if (a_event[1]==0 & a_event[2]==0){
        sapply((grace_period_duration+1):max.interval, function(i){
          ids <- dat_y_new$id.new[dat_y_new$event_y==0 & dat_y_new$max_int>=i]
          ix <- dat_a_grace$id.new %in% ids
          sum(dat_a_grace$w_0_prod_trim[ix] * dat_a_grace$Freq[ix], na.rm=TRUE)
        })
      }
    }
  }
  
  
  
  if(parallel){
    d_list <- parallel::mclapply(setNames(c(1:4), c("d_11", "d_10", "d_01","d_00")), function(x) {
      lookup <- data.frame(a=c(1,1,0,0), event=c(1,0,1,0))
      compute_outcome_weights(a_event = as.numeric(lookup[x,]))
    },
    mc.cores=4)
  } else if (!parallel){
    d_list <- pbapply::pblapply(setNames(c(1:4), c("d_11", "d_10", "d_01","d_00")), function(x) {
      lookup <- data.frame(a=c(1,1,0,0), event=c(1,0,1,0))
      compute_outcome_weights(a_event = as.numeric(lookup[x,]))
    })
  }
  
  # combine
  print("completed outcome weights"); print(Sys.time())
  option_warn <- getOption("warn")
  options(warn = 2)
  dat_y_m <- list()
  dat_y_m$dat_y_m_1 <- data.table(interval = rep(1:max.interval, times=2),
                                  weights = c(d_10.first, d_list$d_10,
                                              d_11.first, d_list$d_11),
                                  event_y = rep(0:1, each=max.interval)
  )[,tstart.new := (interval-1)*30]
  dat_y_m$dat_y_m_0 <- data.table(interval = rep(1:max.interval, times=2),
                                  weights = c(d_00.first, d_list$d_00,
                                              d_01.first, d_list$d_01),
                                  event_y = rep(0:1, each=max.interval)
  )[,tstart.new := (interval-1)*30]
  options(warn = option_warn)
  return(dat_y_m)
}
