gp_outcome_models_function <- function(dat_outcome, t.splines, followup_years){
  m_1 <- speedglm::speedglm(family=quasibinomial(), data=dat_outcome$dat_y_m_1,
                            formula = as.formula(paste0("event_y ~ ", t.splines)),
                            weights = dat_outcome$dat_y_m_1$weights, sparse=FALSE) %>% strip.glm(.)
  
  m_0 <- speedglm::speedglm(family=quasibinomial(), data=dat_outcome$dat_y_m_0,
                            formula = as.formula(paste0("event_y ~ ", t.splines)),
                            weights = dat_outcome$dat_y_m_0$weights,sparse=FALSE) %>% strip.glm(.)
  
  d_temp <- bind_rows(dat_outcome$dat_y_m_0[, .(event_y, tstart.new, weights, group.binary=0)],
                      dat_outcome$dat_y_m_1[, .(event_y, tstart.new, weights, group.binary=1)])
  
  m.hr <- speedglm::speedglm(family=quasibinomial(), data=d_temp,
                             formula = as.formula(paste0("event_y ~ group.binary + ", t.splines)),
                             weights = d_temp$weights, sparse=FALSE) %>% strip.glm(.)
  output <- list()
  output$ate <- cbind(predict(m_1, 
                              newdata=data.frame(tstart.new=seq(0,followup_years*12*30,by=30),
                                                 group.binary=1),
                              type="response") %>% {1-.} %>% cumprod %>% {1-.},
                      predict(m_0, 
                              newdata=data.frame(tstart.new=seq(0,followup_years*12*30,by=30),
                                                 group.binary=0),
                              type="response") %>% {1-.} %>% cumprod %>% {1-.}
  ) %>% as.data.frame %>% mutate(month=row_number()) %>% rename(ci_1=V1, ci_0=V2)
  output$ate.discrete_time_hazards <- data.frame("surgery"=predict(m.hr, 
                                                                   newdata=data.frame(tstart.new=seq(0,followup_years*12*30,by=30),
                                                                                      group.binary=1),
                                                                   type="response"),
                                                 "no.surgery"=predict(m.hr, 
                                                                      newdata=data.frame(tstart.new=seq(0,followup_years*12*30,by=30),
                                                                                         group.binary=0),
                                                                      type="response") 
  )
  output$ate.hr <- mean(output$ate.discrete_time_hazards[,"surgery"])/mean(output$ate.discrete_time_hazards[,"no.surgery"])
  return(output)
}
