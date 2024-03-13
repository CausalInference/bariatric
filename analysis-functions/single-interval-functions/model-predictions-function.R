# model predictions
model_predictions <- function(dat_outcome, mat, stabilized=TRUE, print_coef=FALSE, trim=TRUE){
  t_d <- speedglm::speedglm.wfit(y=dat_outcome$y_surgery[dat_outcome$eligible==1],
                                 X=mat[dat_outcome$eligible==1,],
                                 weights=dat_outcome$Freq[dat_outcome$eligible==1],
                                 family=binomial())
  dat_outcome$pred_t_d_vector <- plogis(mat %*% coef(t_d))
  dat_outcome$pred_t_n_vector <- mean(dat_outcome$y_surgery[dat_outcome$eligible==1])
  if(!stabilized){
    dat_outcome[
      , w_t := fcase(y_surgery==1, 1 / pred_t_d_vector,
                     y_surgery==0, 1 / (1-pred_t_d_vector)
      )][
        , w := w_t]
  } else if(stabilized){
    dat_outcome[
      , w_t := fcase(y_surgery==1, pred_t_n_vector / pred_t_d_vector,
                     y_surgery==0, (1-pred_t_n_vector) / (1-pred_t_d_vector)
      )][
        , w := w_t]
  }
  
  
  tau <- quantile(dat_outcome$w[dat_outcome$y_surgery==1], probs=0.99)
  if(trim){
    dat_outcome$w <- pmin(dat_outcome$w, max(10, tau))
  }
  return(dat_outcome)
}
