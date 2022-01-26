#' onechange Optimal Partitioning
#'
#'
#' @description Optimal Partitioning Algorithm for time series with 0/1 changepoint
#'
#' @param data vector of data points
#' @param cost type of my cost (gauss or poisson)
#' @param beta penalty value
#'
#' @return a changepoint, a global cost
#' @export
#'
#' @examples
#' onechangeOP(c(0,0,0,1,1,1,1), beta = 0.00001)
#' onechangeOP(c(rnorm(50, mean = 0, sd = 1), rnorm(50, mean = 10, sd = 1)))
#' onechangeOP(data_generator(25, chpts = 10, means = c(20,0), type = "gauss"), beta = 5)
onechangeOP <- function(data, cost = "bernoulli", beta = best_beta(data))
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
  
  for (t in 2:n)
  {
    val_min <- cost_f(data[1:t])
    arg_min <- 0
    for (s in 2:t)
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
  }
  
  # backtracking
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




#' Optimal Partitioning
#'
#'
#' @description Optimal Partitioning Algorithm
#'
#' @param data vector of data points
#' @param cost type of my cost (gauss or poisson)
#' @param beta penalty value
#'
#' @return a vector of changepoints, a global cost
#' @export
#'
#' @examples
#' myOP(c(0,0,1,1,0,0,0), beta = 0.00001)
#' myOP(c(rnorm(50, mean = 0, sd = 0), rnorm(50, mean = 10, sd = 0)))
#' myOP(data_generator(25, chpts = c(10,20), means = c(20,0,20), type = "gauss"), beta = 5)
myOP <- function(data, cost = "bernoulli", beta = best_beta(data))
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
  
  for (t in 2:n)
  {
    val_min <- cost_f(data[1:t])
    arg_min <- 0
    for (s in 2:t)
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
  }
  
  # backtracking
  v <- cp[n]
  P <- cp[n]
  
  while (v > 0)
  {
    P <- c(P, cp[v])
    v <- cp[v]
  }
  P <- rev(P)[-1]
  
  return(list(changepoints = P, globalCost = Q[n] - length(P)*beta))
}

