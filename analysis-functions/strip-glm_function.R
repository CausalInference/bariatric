# trim model function
strip.glm <- function(model){
  model$y = c()
  model$model = c()
  
  model$residuals = c()
  model$fitted.values = c()
  model$effects = c()
  model$qr$qr = c()
  model$linear.predictors = c()
  model$weights = c()
  model$prior.weights = c()
  model$data = c()
  
  model$family$variance = c()
  model$family$dev.resids = c()
  model$family$aic = c()
  model$family$validmu = c()
  model$family$simulate = c()
  
  return(model)
}
