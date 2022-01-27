dataSeries2 <- function(n0 = 100, n1 = 100, p0 = 0.3, p1 = 0.8){
  s1 <- rbinom(n = n0, size = 1, prob = p0)
  s2 <- rbinom(n = n1, size = 1, prob = p1)
  return(c(s1,s2))
}

#' simu_EMV
#'
#'
#' @description simulates nsimu samples and applies the simu_EMV method to each of them.
#' 
#'
#' @param nsimu number simulations
#' @param tresh the treshold parameter for simu_EMV
#' @param tau_choice tau_choice wether you want to chose or not the place at which the fraud will occur for each row of the matrix
#' 
#' @return a tibble sumarizing how well did the model run. Each column corresponds to the performance metrics, with 19 rows beeing the places when the fraud event occured (at 100, 200, ..., 1800, 1900).
#' 
#' @export
#'
#' @examples
#' 
#' simu_EMV(nsimu = 50)
#' 
#' 
simu_EMV <- function(nsimu = 10, tresh = 0.2, tau_choice = TRUE){
  
  compteur <- 1
  
  #Tableur de résultats
  res = tibble(tau = rep(0,19),
               tpr = rep(0,19),
               accuracy = rep(0,19),
               tss = rep(0,19),
               tau_dist = rep(0,19),
               f1 = rep(0,19),
               sensi = rep(0,19),
               specif = rep(0,19)
               
  )
  
  #Pour chaque position de tau
  for(tau in seq(100,1900, by = 100)){
    
    
    
    if(tau_choice){data <- construct_data(n = 2000, chosen_tau = tau, nsimu = nsimu, tau_choice = tau_choice)}
    else{data <- construct_data(n = 2000, nsimu = nsimu, tau_choice = tau_choice)}
    
    X <- data$data
    
    #Vecteur de fraudes réel
    true_vec <- data$labels
    
    #Vecteur de fraude estimé
    #---EMV:
    estim_vec_EMV <- c()
    tau_dist_list <- c()
    
    #On réalise nsimu simulations de données
    for(i in (1:nsimu)){
      
      #Estimations
      #--EMV
      res_EMV <- tau_EMV_K(data = X[i,], tresh = tresh, K = 1)
      
      estim_vec_EMV <- c(estim_vec_EMV, res_EMV$Fraude)
      
      tau_hat <- res_EMV$tau_estim
      
      tau_dist_list <- c(tau_dist_list, abs(tau_hat-tau))
      
      
    }
    
    res$tpr[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tp_rate")$score
    res$accuracy[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "accuracy")$score
    res$tss[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tss")$score
    res$tau_dist[compteur] <- mean(tau_dist_list)
    res$sensi[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "sensibilite")$score
    res$specif[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "specificite")$score
    res$f1[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "f1")$score
    
    compteur <- compteur+1
    
  }
  
  return(res)
  
}

#' simu_EMV_K
#'
#'
#' @description simulates nsimu samples and applies the simu_EMV_K method to each of them.
#' 
#'
#' @param nsimu number simulations
#' @param tresh the treshold parameter for simu_EMV_K
#' @param K the K parameter for simu_EMV_K
#' @param tau_choice tau_choice wether you want to chose or not the place at which the fraud will occur for each row of the matrix
#' 
#' @return a tibble sumarizing how well did the model run. Each column corresponds to the performance metrics, with 19 rows beeing the places when the fraud event occured (at 100, 200, ..., 1800, 1900).
#' 
#' @export
#'
#' @examples
#' 
#' simu_EMV_K(nsimu = 50)
#' 
#' 
simu_EMV_K <- function(nsimu = 10, tresh = 0.2, K = 1, tau_choice = TRUE){
  
  compteur <- 1
  
  #Tableur de résultats
  res = tibble(tau = rep(0,19),
               tpr = rep(0,19),
               accuracy = rep(0,19),
               tss = rep(0,19),
               tau_dist = rep(0,19),
               f1 = rep(0,19),
               sensi = rep(0,19),
               specif = rep(0,19)
               
  )
  
  #Pour chaque position de tau
  for(tau in seq(100,1900, by = 100)){
    
    
    if(tau_choice){data <- construct_data(n = 2000, chosen_tau = tau, nsimu = nsimu, tau_choice = tau_choice)}
    else{data <- construct_data(n = 2000, nsimu = nsimu, tau_choice = tau_choice)}
    
    X <- data$data
    
    #Vecteur de fraudes réel
    true_vec <- data$labels
    
    #Vecteur de fraude estimé
    #---EMV:
    estim_vec_EMV <- c()
    tau_dist_list <- c()
    
    #On réalise nsimu simulations de données
    for(i in (1:nsimu)){
      
      #Estimations
      #--EMV
      res_EMV <- tau_EMV_K(data = X[i,], tresh = tresh, K = K)
      
      estim_vec_EMV <- c(estim_vec_EMV, res_EMV$Fraude)
      
      tau_hat <- res_EMV$tau_estim
      
      tau_dist_list <- c(tau_dist_list, abs(tau_hat-tau))
      
    }
    
    res$tpr[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tp_rate")$score
    res$accuracy[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "accuracy")$score
    res$tss[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tss")$score
    res$tau_dist[compteur] <- mean(tau_dist_list)
    res$sensi[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "sensibilite")$score
    res$specif[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "specificite")$score
    res$f1[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "f1")$score
    
    
    compteur <- compteur+1
    
  }
  
  return(res)
  
}

#' simu_EMV_K_unif
#'
#'
#' @description simulates nsimu samples and applies the simu_EMV_K_unif method to each of them.
#' 
#'
#' @param nsimu number simulations
#' @param tresh the treshold parameter for simu_EMV_K_unif
#' @param K the K parameter for simu_EMV_K_unif
#' @param tau_choice tau_choice wether you want to chose or not the place at which the fraud will occur for each row of the matrix
#' 
#' @return a tibble sumarizing how well did the model run. Each column corresponds to the performance metrics, with 19 rows beeing the places when the fraud event occured (at 100, 200, ..., 1800, 1900).
#' 
#' @export
#'
#' @examples
#' 
#' simu_EMV_K_unif(nsimu = 50)
#' 
#'  
simu_EMV_K_unif <- function(nsimu = 10, tresh = 0.2, K = 1, tau_choice = TRUE){
  
  compteur <- 1
  
  #Tableur de résultats
  res = tibble(tau = rep(0,19),
               tpr = rep(0,19),
               accuracy = rep(0,19),
               tss = rep(0,19),
               tau_dist = rep(0,19),
               f1 = rep(0,19),
               sensi = rep(0,19),
               specif = rep(0,19)
               
  )
  
  #Pour chaque position de tau
  for(tau in seq(100,1900, by = 100)){
    
    if(tau_choice){data <- construct_data(n = 2000, chosen_tau = tau, nsimu = nsimu, tau_choice = tau_choice)}
    else{data <- construct_data(n = 2000, nsimu = nsimu, tau_choice = tau_choice)}
    
    X <- data$data
    
    #Vecteur de fraudes réel
    true_vec <- data$labels
    
    #Vecteur de fraude estimé
    #---EMV:
    estim_vec_EMV <- c()
    tau_dist_list <- c()
    
    #On réalise nsimu simulations de données
    for(i in (1:nsimu)){
      
      #Estimations
      #--EMV
      res_EMV <- tau_EMV_K_unif(data = X[i,], tresh = tresh, K = K)
      
      estim_vec_EMV <- c(estim_vec_EMV, res_EMV$Fraude)
      
      tau_hat <- res_EMV$tau_estim
      
      tau_dist_list <- c(tau_dist_list, abs(tau_hat-tau))
      
    }
    
    res$tpr[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tp_rate")$score
    res$accuracy[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "accuracy")$score
    res$tss[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tss")$score
    res$tau_dist[compteur] <- mean(tau_dist_list)
    res$sensi[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "sensibilite")$score
    res$specif[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "specificite")$score
    res$f1[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "f1")$score
    
    compteur <- compteur+1
    
  }
  
  return(res)
  
}

#' simu_EMV_K_norm
#'
#'
#' @description simulates nsimu samples and applies the EMV_K_norm method to each of them.
#' 
#'
#' @param nsimu number simulations
#' @param tresh the treshold parameter for EMV_K_norm
#' @param K the K parameter for EMV_K_norm
#' @param tau_choice tau_choice wether you want to chose or not the place at which the fraud will occur for each row of the matrix
#' 
#' @return a tibble sumarizing how well did the model run. Each column corresponds to the performance metrics, with 19 rows beeing the places when the fraud event occured (at 100, 200, ..., 1800, 1900).
#' 
#' @export
#'
#' @examples
#' 
#' simu_EMV_K_norm(nsimu = 50)
#' 
#' 
simu_EMV_K_norm <- function(nsimu = 10, tresh = 0.2, K = 1, tau_choice=TRUE){
  
  compteur <- 1
  
  #Tableur de résultats
  res = tibble(tau = rep(0,19),
               tpr = rep(0,19),
               accuracy = rep(0,19),
               tss = rep(0,19),
               tau_dist = rep(0,19),
               f1 = rep(0,19),
               sensi = rep(0,19),
               specif = rep(0,19)
               
  )
  
  #Pour chaque position de tau
  for(tau in seq(100,1900, by = 100)){
    
    if(tau_choice){data <- construct_data(n = 2000, chosen_tau = tau, nsimu = nsimu, tau_choice = tau_choice)}
    else{data <- construct_data(n = 2000, nsimu = nsimu, tau_choice = tau_choice)}
    
    X <- data$data
    
    #Vecteur de fraudes réel
    true_vec <- data$labels
    
    #Vecteur de fraude estimé
    #---EMV:
    estim_vec_EMV <- c()
    tau_dist_list <- c()
    
    #On réalise nsimu simulations de données
    for(i in (1:nsimu)){
      
      #Estimations
      #--EMV
      res_EMV <- tau_EMV_K_norm(data = X[i,], tresh = tresh, K = K)
      
      estim_vec_EMV <- c(estim_vec_EMV, res_EMV$Fraude)
      
      tau_hat <- res_EMV$tau_estim
      
      tau_dist_list <- c(tau_dist_list, abs(tau_hat-tau))
      
    }
    
    res$tpr[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tp_rate")$score
    res$accuracy[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "accuracy")$score
    res$tss[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "tss")$score
    res$tau_dist[compteur] <- mean(tau_dist_list)
    res$sensi[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "sensibilite")$score
    res$specif[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "specificite")$score
    res$f1[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_EMV, score = "f1")$score
    
    compteur <- compteur+1
    
  }
  
  return(res)
  
}

#' simu_CSM
#'
#'
#' @description simulates nsimu samples and applies the cusum method to each of them.
#' 
#'
#' @param nsimu number simulations
#' @param tau_choice wether you want to chose or not the place at which the fraud will occur for each row of the matrix
#' 
#'
#' 
#' @return a tibble sumarizing how well did the model run. Each column corresponds to the performance metrics, with 19 rows beeing the places when the fraud event occured (at 100, 200, ..., 1800, 1900).
#' 
#' @export
#'
#' @examples
#' 
#' simu_CSM(nsimu = 50)
#' 
#' 
simu_CSM <- function(nsimu = 10,  tau_choice = TRUE){
  
  compteur <- 1
  
  #Tableur de résultats
  res = tibble(tau = rep(0,19),
               tpr = rep(0,19),
               accuracy = rep(0,19),
               tss = rep(0,19),
               tau_dist = rep(0,19),
               f1 = rep(0,19),
               sensi = rep(0,19),
               specif = rep(0,19)
               
  )
  
  #Pour chaque position de tau
  for(tau in seq(100,1900, by = 100)){
    
    if(tau_choice){data <- construct_data(n = 2000, chosen_tau = tau, nsimu = nsimu, tau_choice = tau_choice)}
    else{data <- construct_data(n = 2000, nsimu = nsimu, tau_choice = tau_choice)}
    
    X <- data$data
    
    #Vecteur de fraudes réel
    true_vec <- data$labels
    
    #Vecteur de fraude estimé
    
    #---CUSUM:
    estim_vec_CSM <- c()
    tau_dist_list <- c()
    
    #On réalise nsimu simulations de données
    for(i in (1:nsimu)){
      
      #Estimations
      #--CUSUM
      
      cusum_res <- cusum(X = X[i,])
      
      tau_hat <- cusum_res$tau
      tau_dist_list <- c(tau_dist_list, abs(tau_hat-tau))
      
      cusum_pred <- cusum_res$y_pred
      
      if(cusum_pred == 1){estim_vec_CSM = c(estim_vec_CSM,TRUE)}
      else{estim_vec_CSM=c(estim_vec_CSM, FALSE)}
    }
    
    res$tpr[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_CSM, score = "tp_rate")$score
    res$accuracy[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_CSM, score = "accuracy")$score
    res$tss[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_CSM, score = "tss")$score
    res$tau_dist[compteur] <- mean(tau_dist_list)
    res$sensi[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_CSM, score = "sensibilite")$score
    res$specif[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_CSM, score = "specificite")$score
    res$f1[compteur] <- compute_mf(v_ver = true_vec, v_est = estim_vec_CSM, score = "f1")$score
    
    compteur <- compteur+1
    
  }
  
  return(res)
  
}


construct_data = function(n=2000,tau_choice = TRUE, chosen_tau, nsimu) {
  
  p1 = runif(nsimu,min=0.1,max=0.9)
  p2 = runif(nsimu,min=0.1,max=0.9)
  
  id = rbinom(nsimu,size=1,prob=0.5)
  p2[id==0] = p1[id==0]
  
  if(tau_choice){
    tau <- chosen_tau
  
    Y_true = !(p1 == p2)
    
    mat_X = matrix(data=NA, nrow = nsimu, ncol=n)
    
    for(i in 1:nsimu) {
      x1 = rbinom(n=tau,size=1,prob=p1[i])
      x2 = rbinom(n=n-tau,size=1,prob=p2[i])
      mat_X[i,] = c(x1, x2)
    }
  }
  else{
    tau = rdunif(nsimu,1,9)*100
  
    Y_true = !(p1 == p2)
  
    mat_X = matrix(data=NA, nrow = nsimu, ncol=n)
  
    for(i in 1:nsimu) {
      x1 = rbinom(n=tau[i],size=1,prob=p1[i])
      x2 = rbinom(n=n-tau[i],size=1,prob=p2[i])
      mat_X[i,] = c(x1, x2)
    }
  }
  
  
  return(list("data" = mat_X, "labels" = Y_true))
}

