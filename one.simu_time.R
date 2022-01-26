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

one.simu_time_ocOP <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(onechangeOP(data))[3])
  
  return(sim)
}

one.simu_time_ocPELT <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(onechangePELT(data))[3])
  
  return(sim)
}


one.simu_time_OP <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(myOP(data))[3])
  
  return(sim)
}

one.simu_time_PELT <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(myPELT(data))[3])
  
  return(sim)
}


one.simu_time_CUSUM <- function(K,n)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(cusum(data))[3])
  
  return(sim)
}

one.simu_time_TRV <- function(K,n)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(print("En cours"))[3])
  
  return(sim)
}

one.simu_time_EMV <- function(K,n)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(tau_EMV(data, tresh=0.5))[3])
  
  return(sim)
}