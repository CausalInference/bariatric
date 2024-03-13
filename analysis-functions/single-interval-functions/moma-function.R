# model matrix 
moma <- function(dat, ipw.formula_d){
  return(model.matrix(as.formula(ipw.formula_d), dat=dat))
}