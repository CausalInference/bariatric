gp_weights_function <- function(dat_a, min.interval, grace.intervals, trim){
  dat_a[interval < min.interval, w_t_1_vector := fcase(
    y_surgery==0 & y_surgery_lag==0, 1, 
    y_surgery==1, 0
  )]
  
  dat_a[, interval_temp := interval - min.interval + 1]
  dat_a[interval >= min.interval, w_t_1_vector := fcase(
    eligible==0, 1,
    eligible==1 & y_surgery_lag==1, 1,
    eligible==1 & y_surgery_lag==0 & y_surgery==1, pred_t_n_vector*numerator_function(j=interval_temp, A=1, 
                                                                                      m=grace.intervals - min.interval + 1) / t_d_preds,
    eligible==1 & y_surgery_lag==0 & y_surgery==0, pred_t_n_vector*numerator_function(j=interval_temp, A=0, 
                                                                                      m=grace.intervals - min.interval + 1) / (1-t_d_preds)
  )]
  
  dat_a[,w_t_0_vector := fcase(
    eligible==0 & interval==1, 0,
    eligible==0 & interval>1, 1,
    eligible==1 & y_surgery==0, pred_t_n_vector / (1-t_d_preds),
    eligible==1 & y_surgery==1, 0
  )]
  
  w_1_prod_vector <- ave(dat_a$w_t_1_vector, dat_a$id.new, FUN=cumprod)
  w_0_prod_vector <- ave(dat_a$w_t_0_vector, dat_a$id.new, FUN=cumprod)
  
  tau_1a <- quantile(w_1_prod_vector[dat_a$y_surgery==1], probs=0.995); 
  tau_1b <- quantile(w_1_prod_vector[w_1_prod_vector>0], probs=0.0001); 
  
  tau_0a <- quantile(w_0_prod_vector[w_0_prod_vector>0], probs=0.995) 
  tau_0b <- quantile(w_0_prod_vector[w_0_prod_vector>0], probs=0.0001)
  
  weights_list <- list()
  if(trim){
    weights_list$w_1_prod_trim_vector <- pmin(w_1_prod_vector, min(5000, tau_1a))
    weights_list$w_0_prod_trim_vector <- pmin(w_0_prod_vector, min(5000, tau_1a))
  }
  if(!trim){
    weights_list$w_1_prod_trim_vector <- w_1_prod_vector
    weights_list$w_0_prod_trim_vector <- w_0_prod_vector
  }
  return(weights_list)
}
