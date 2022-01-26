#' stock_v
#'
#'
#' @description This function allows us to stock in a vector data that came from a set of simulations with the mclapply function
#'
#'
#' @param simu a vector of simulations
#'
#' @return a vector of simulations
#' @export
#'
#' @examples
#' print("Not necessary")
stock_v <- function(simu)
{
  n <- length(simu)
  vec <- NULL
  for (i in 1:n)
  {
    vec[i] <- simu[[i]]
  }
  return(vec)
}

#' one.simu_time_ocOP
#'
#'
#' @description one simulation function for onechange OP
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_ocOP(2, 1000)
one.simu_time_ocOP <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(onechangeOP(data))[3])
  
  return(sim)
}

#' one.simu_time_ocPELT
#'
#'
#' @description one simulation function for onechange PELT
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_ocPELT(2, 1000)
one.simu_time_ocPELT <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(onechangePELT(data))[3])
  
  return(sim)
}

#' one.simu_time_OP
#'
#'
#' @description one simulation function for OP
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_OP(2, 1000)
one.simu_time_OP <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(myOP(data))[3])
  
  return(sim)
}

#' one.simu_time_PELT
#'
#'
#' @description one simulation function for  PELT
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_PELT(2, 1000)
one.simu_time_PELT <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(myPELT(data))[3])
  
  return(sim)
}

#' one.simu_time_CUSUM
#'
#'
#' @description one simulation function for CUSUM
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_CUSUM(2, 1000)
one.simu_time_CUSUM <- function(K,n)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(cusum(data))[3])
  
  return(sim)
}

#' one.simu_time_TRV
#'
#'
#' @description one simulation function for TRV
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_TRV(2, 1000)
one.simu_time_TRV <- function(K,n)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(print("En cours"))[3])
  
  return(sim)
}

#' one.simu_time_EMV
#'
#'
#' @description one simulation function for EMV
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_EMV(2, 1000)
one.simu_time_EMV <- function(K,n)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(tau_EMV(data, tresh=0.5))[3])
  
  return(sim)
}
