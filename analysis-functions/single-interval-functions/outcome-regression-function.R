outcome_reg <- function(dat_outcome, followup_years, weight_name){
  setDT(dat_outcome)
  setkey(dat_outcome, id.new, tstart.new)
  dat_outcome[,months := tstart.new/30]
  
  if(is.null(weight_name)){
    dat_outcome$weights <- 1
  } else {
    if(is.null(dat_outcome$Freq)){dat_outcome$Freq <- 1}
    dat_outcome$weights <- dat_outcome[[weight_name]]*dat_outcome$Freq
  }
  max.interval=(followup_years)*12
  tempw_00a <- unlist(lapply((0:max.interval)*30, function(x) dat_outcome[event_y==0 & y_surgery==0 & tstart.new>=x, sum(weights)]))	
  tempw_00b <- unlist(lapply((0:max.interval)*30, function(x) dat_outcome[event_y==1 & y_surgery==0 & tstart.new>x,  sum(weights)]))	
  tempw_01a <- unlist(lapply((0:max.interval)*30, function(x) dat_outcome[event_y==0 & y_surgery==1 & tstart.new>=x, sum(weights)]))		
  tempw_01b <- unlist(lapply((0:max.interval)*30, function(x) dat_outcome[event_y==1 & y_surgery==1 & tstart.new>x,  sum(weights)]))	
  tempw_10 <- unlist(lapply((0:max.interval)*30, function(x)  dat_outcome[event_y==1 & y_surgery==0 & tstart.new==x, sum(weights)]))	
  tempw_11 <- unlist(lapply((0:max.interval)*30, function(x)  dat_outcome[event_y==1 & y_surgery==1 & tstart.new==x, sum(weights)]))	
  
  grid <- setDT(expand.grid(tstart.new=seq(from=0, to=max.interval*30, by=30), event_y=0:1, y_surgery=0:1)) 
  grid$w <- NA
  grid$w[grid$event_y==0 & grid$y_surgery==0] <- tempw_00a + tempw_00b
  grid$w[grid$event_y==0 & grid$y_surgery==1] <- tempw_01a + tempw_01b
  grid$w[grid$event_y==1 & grid$y_surgery==0] <- tempw_10
  grid$w[grid$event_y==1 & grid$y_surgery==1] <- tempw_11
  print("weights done")
  
  m <- speedglm::speedglm(family=quasibinomial(),
                          data=grid,
                          weights=grid$w,
                          event_y ~ y_surgery*ns(tstart.new, knots=c(2*12*30,3.5*12*30,5*12*30), Boundary.knots=c(0.5*12*30,6*12*30)) 
  )
  m_hr <- speedglm::speedglm(family=quasibinomial(),
                          data=grid,
                          weights=grid$w,
                          event_y ~ y_surgery + ns(tstart.new, knots=c(2*12*30,3.5*12*30,5*12*30), Boundary.knots=c(0.5*12*30,6*12*30))
  )
  output <- list()
  output$ci <- cbind(predict(m, newdata=data.frame(tstart.new=seq(30,max.interval*30,by=30), y_surgery=1), type="response") %>% {1-.} %>% cumprod %>% {1-.},
                                  predict(m, newdata=data.frame(tstart.new=seq(30,max.interval*30,by=30),y_surgery=0), type="response") %>% {1-.} %>% cumprod %>% {1-.}
  ) %>% as.data.frame %>% mutate(month=row_number()) %>% rename(ci_1=V1, ci_0=V2)
  
  output$dth <- data.frame("treat1"=predict(m_hr, newdata=data.frame(tstart.new=seq(30,followup_years*12*30,by=30), y_surgery=1), type="response"),
                           "treat0"=predict(m_hr, newdata=data.frame(tstart.new=seq(30,followup_years*12*30,by=30), y_surgery=0), type="response") 
  )	
  output$hr <- mean(output$dth[,"treat1"])/mean(output$dth[,"treat0"])
  output$point_est <- output$ci[followup_years*12,]
  output$rd <- output$point_est$ci_1 - output$point_est$ci_0
  return(output)
}

