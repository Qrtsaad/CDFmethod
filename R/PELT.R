#' Pruned Exact Linear Time
#'
#'
#' @description Pruned Exact Linear Time Algorithm
#'
#' @param data vector of data points
#' @param cost a number
#' @param beta a number
#'
#' @return a vector of changepoints, global cost
#' @export
#'
#' @examples
#' myPELT(c(0,0,1,1,0,0,0), beta = 0.00001)
#' myPELT(c(rnorm(50, mean = 0, sd = 0), rnorm(50, mean = 10, sd = 0)), beta = 1)
#' myPELT(data_generator(25, chpts = c(10,20), means = c(20,0,20), type = "gauss"), beta = 5)

onechangePELT <- function(data, cost = "bernoulli", beta = best_beta(data), eps = 1e-6)
{
  allowed.cost <- c("gauss", "poisson", "negbin", "bernoulli")
  if(!cost %in% allowed.cost){stop('type must be one of: ', paste(allowed.cost, collapse=", "))}
  
  if (cost == "gauss") {cost_f <- cost_gauss}
  else if (cost == "poisson") {cost_f <- cost_poiss}
  else if (cost == "negbin")
  {
    cost_f <- cost_negbin
  }
  else if (cost == 'bernoulli') {cost_f <- cost_bernoulli}
  
  n <- length(data)
  
  cp <- rep(0,n)
  Q <- rep(0,n)
  R <- NULL
  
  for (t in 2:n)
  {
    val_min <- cost_f(data[1:t])
    arg_min <- 0
    R <- c(R,t)
    
    for (s in R)
    {
      a <- Q[s-1] + cost_f(data[s:t]) + beta
      if (a < val_min)
      {
        val_min <- a
        arg_min <- s - 1
      }
    }
    
    Q[t] <- val_min
    cp[t] <- arg_min
    
    for (s in R){
      if (Q[t] <= Q[s-1] + cost_f(data[s:t])){
        R <- R[R!= s]}}
  }
  
  #backtracking
  v <- cp[n]
  P <- cp[n]
  
  while (v > 0)
  {
    P <- c(P, cp[v])
    v <- cp[v]
  }
  P <- rev(P)[-1]
  
  
  #selection of the minimal changepoint 
  val_min <- +Inf
  for (i in P)
  {
    a <- cost_f(data[1:i]) + cost_f(data[(i+1):n]) + beta
    if (a < val_min)
    {
      val_min <- a
      arg_min <- i
    }
  }
  
  
  if(arg_min == 2){rupt <- NULL} else {rupt <- arg_min} # Si on detecte le min en 2 alors c'est des données sans ruptures car ca impliquerait que l'on ai un saut en 1 or c'est la premiere donnée
  
  return(list(tau = rupt, globalCost = Q[n] - beta))
}
