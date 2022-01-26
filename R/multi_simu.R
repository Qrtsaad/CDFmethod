multi_cusum = function(mat_X,a=0.05) {
  nb_test = DIM(mat_X)[1]
  y_pred = tau = p_value = times = rep(NA,nb_test)

  for (j in 1:nb_test) {
    CSM = cusum(mat_X[j,],a)
    
    y_pred[j] = CSM$y_pred
    p_value[j] = CSM$p_value
    tau[j] = CSM$tau
    times[j] = CSM$time
  }
  return(list("y_pred"=y_pred,"p_value"=p_value,"tau"=tau,"times"=times))
}


multi_ocOP = function(mat_X,a=0.05) {
  nb_test = DIM(mat_X)[1]
  tau = globalCost = rep(NA,nb_test)

  for (j in 1:nb_test) {
    OP = ocOP(mat_X[j,],a)
    
    tau[j] = OP$tau
    globalCost[j] = OP$globalCost
  }
  return(list("tau"=tau, "globalCost"=globalCost))
}



multi_ocPELT = function(mat_X,a=0.05) {
  nb_test = DIM(mat_X)[1]
  tau = globalCost = rep(NA,nb_test)

  for (j in 1:nb_test) {
    PELT = ocPELT(mat_X[j,],a)
    
    tau[j] = PELT$tau
    globalCost[j] = PELT$globalCost
  }
  return(list("tau"=tau, "globalCost"=globalCost))
}



multi_OP = function(mat_X,a=0.05) {
  nb_test = DIM(mat_X)[1]
  tau = globalCost = rep(NA,nb_test)
  
  for (j in 1:nb_test) {
    OP = myOP(mat_X[j,],a)
    
    tau[j] = OP$tau
    globalCost[j] = OP$globalCost
  }
  return(list("tau"=tau, "globalCost"=globalCost))}



multi_PELT = function(mat_X,a=0.05) {
  nb_test = DIM(mat_X)[1]
  tau = globalCost = rep(NA,nb_test)

  for (j in 1:nb_test) {
    PELT = myPELT(mat_X[j,],a)
    
    tau[j] = PELT$tau
    globalCost[j] = PELT$globalCost
  }
  return(list("tau"=tau, "globalCost"=globalCost))
}
