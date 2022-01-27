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

#' one.simu_time_ocOPcpp
#'
#'
#' @description one simulation function for onechange OPcpp
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_ocOPcpp(2, 1000)
one.simu_time_ocOPcpp <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(onechangeOPcpp(data))[3])
  
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

#' one.simu_time_ocPELTcpp
#'
#'
#' @description one simulation function for onechange PELTcpp
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_ocPELTcpp(2, 1000)
one.simu_time_ocPELTcpp <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(onechangePELTcpp(data))[3])
  
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

#' one.simu_time_OPcpp
#'
#'
#' @description one simulation function for OPcpp
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_OPcpp(2, 1000)
one.simu_time_OPcpp <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(myOPcpp(data))[3])
  
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

#' one.simu_time_PELTcpp
#'
#'
#' @description one simulation function for PELTcpp
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_PELTcpp(2, 1000)
one.simu_time_PELTcpp <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(myPELTcpp(data))[3])
  
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

#' one.simu_time_CUSUMcpp
#'
#'
#' @description one simulation function for CUSUMcpp
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_CUSUMcpp(2, 1000)
one.simu_time_CUSUMcpp <- function(K,n,a=0.05)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(cusumcpp(data,a))[3])
  
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

#' one.simu_time_EMVcpp
#'
#'
#' @description one simulation function for EMVcpp
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_EMVcpp(2, 1000)
one.simu_time_EMVcpp <- function(K,n)
{
  data <- dataSeries(K, n = 2000, probs = sample(1:9,K)/10)
  sim <- as.numeric(system.time(tau_EMVcpp(data, tresh=0.5))[3])
  
  return(sim)
}



#' one.simu_RF
#'
#'
#' @description one simulation function for Random Forest
#'
#' 
#' @param n size of the time series
#'
#' @return nothing
#' @export
#'
#' @examples
#' one.simu_RF(2000)
one.simu_RF <- function(n)
{
  nb_test = 500
  p1 = runif(nb_test,min=0.1,max=0.9)
  p2 = runif(nb_test,min=0.1,max=0.9)
  id = rbinom(nb_test,size=1,prob=0.5)
  p2[id==0] = p1[id==0]
  tau = rep(1000,nb_test)
  Y_true = !(p1 == p2)
  Y_pred = rep(0,nb_test)
  
  mat_X = constuct_data(n,tau,p1,p2)
  
  train = sample(nrow(mat_X), 0.7*nrow(mat_X), replace = FALSE)
  TrainSet = mat_X[train,]
  ValidSet = mat_X[-train,]
  
  Y_train = Y_true[train]
  Y_test = Y_true[-train]
  RDM_forest = randomForest(as.factor(Y_train) ~. ,data=TrainSet,ntree=500)
  
  Y_pred_RF = predict(RDM_forest,ValidSet,type="class")
  
}


#' one.simu_time_RF
#'
#'
#' @description one simulation function for get the time of one Random Forest construction
#'
#' 
#' @param n size of the time series
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_time_RF(2000)
one.simu_time_RF <- function(n)
{
  sim <- as.numeric(system.time(one.simu_RF(n))[3])
  
  return(sim)
}
