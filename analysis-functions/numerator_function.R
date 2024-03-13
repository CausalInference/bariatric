numerator_function <- function(j, A, m){
  if(A==1){
    num <- 1 / (m-j+1)
  } else if(A==0){
    num <- (m-j) / (m-j+1)
  }
  return(num)
}

numerator_yearblock_censor_function <- function(j, A, m=12){
  j <- j %% 12; j <- ifelse(j==0,12,j)
  if(A==1){
    num <- 1 / (m-j+1)
  } else if(A==0){
    num <- (m-j) / (m-j+1)
  }
  return(num)
}

beta_numerator_function_utility <- function(m, alpha=2, beta=2){
  j <- 1
  total <- original.total <- 100; grand.total <- 0
  scaled.frac.vector <- NA
  while (j<=m){
    frac <- pbeta(j/m, shape1=alpha, shape2=beta) - pbeta((j-1)/m, shape1=alpha, shape2=beta)
    scaled.frac <- frac*original.total/total
    scaled.frac.vector <- c(scaled.frac.vector, scaled.frac)
    grand.total <- total*scaled.frac + grand.total
    total <- total - total*scaled.frac
    j <- j+1
  }
  return(scaled.frac.vector[-1])
}
beta_numerator_function <- function(j, A, m){
  A*beta_numerator_function_utility(m=m)[j] + (1-A)*(1-beta_numerator_function_utility(m=m)[j])
}


