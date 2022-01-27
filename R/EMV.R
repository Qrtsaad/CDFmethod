#' multi_EMV
#'
#'
#' @description Applies tau_EMV to every row of a given matrix
#'
#' @param X with rows being vectors of Bernoilli data points
#'
#' @return A vector with length équal tu the number of rows of the given matrix. 
#' Each element represents the prediction of fraud for the corresponding row with the tau_EMV method.
#' @export
#'
#' @examples
#' data <- constuct_data2(n = 2000, nsimu = 50)$data
#' 
#' multi_EMV(data)
multi_EMV <- function(mat_X, fun = tau_EMV){
  
  #fun doit prendre pour valeur:
  #tau_EMV _ou_ tau_EMV_K_unif _ou_ tau_EMV_K_norm
  
  nb_test = DIM(mat_X)[1]
  y_pred = rep(NA,nb_test)
  
  for (j in 1:nb_test) {
    result = fun(data = mat_X[j,], tresh = 0.2)
    
    y_pred[j] = result$Fraude
  }
  return(list("y_pred"= y_pred))
}
  
#' multi_EMV_K
#'
#'
#' @description Applies tau_EMV_K to every row of a given matrix
#'
#' @param X with rows being vectors of Bernoilli data points
#'
#' @return A vector with length équal tu the number of rows of the given matrix. 
#' Each element represents the prediction of fraud for the corresponding row with the tau_EMV_K method.
#' @export
#'
#' @examples
#' data <- constuct_data2(n = 2000, nsimu = 4)$data
#' 
#' multi_EMV_(data)
multi_EMV_K <- function( mat_X){
  
  #fun doit prendre pour valeur:
  #tau_EMV _ou_ tau_EMV_K_unif _ou_ tau_EMV_K_norm
  
  nb_test = DIM(mat_X)[1]
  y_pred = rep(NA,nb_test)
  
  for (j in 1:nb_test) {
    result = tau_EMV_K(data = mat_X[j,], tresh = 0.2, K = 100)
    
    y_pred[j] = result$Fraude
  }
  return(list("y_pred"= y_pred))
}  
#' tau_EMV_K
#'
#'
#' @description Detects a fraud in the given data serie using the MLE estimator for the time of fraud.
#'
#' @param data with rows being vectors of Bernoilli data points
#' @param tresh the treshold=abs(\hat{p_0}-\hat{p_1}) that is used to make the decision of whether the sample is a fraud or not.
#' @param K the number of treshold values to skip in order to test 2000/K points instead of 2000. 
#' 
#' @return A tibble summarizing the results of the algorithm 
#' 
#' @export
#'
#' @examples
#' data <- constuct_data2(n = 2000, nsimu = 1)$data
#' 
#' tau_EMV_K(data)
#' 
tau_EMV_K <- function(data, tresh = 0.2, K = 100){
  
  fraude = FALSE
  
  X <- data
  n <- length(X)
  
  tau_EMV_hat <- 0
  tau_V_max <- -3000
  p0_esti <- 0
  p1_esti <- 0
  
  #Pour chaque position de tau possible, on itère:
  for(tau in seq(1, n-1, by = K)){
    
    #Calcul de l'estimateur du maximum de vraissemblance de p0 et p1
    p0_EMV = sum(X[1:tau])/tau
    p1_EMV = sum(X[(tau+1):n])/(n-tau)
    
    #Calcul de la vraissemblance pour chaque tau
    tau_V = tau * log(1-p0_EMV) + (n-tau) * log(1-p1_EMV) + log(p0_EMV/(1-p0_EMV)) * sum(X[1:tau]) + log(p1_EMV/(1-p1_EMV)) * sum(X[(tau+1):n])
    
    
    
    #On ne garde que si la vraissemblance calculée pour le tau courant est supérieure à celle du précédent
    if(!(is.na(tau_V))){
      
      
      if(tau_V >= tau_V_max){
        
        
        #Vraissemblance maximale courrante
        tau_V_max <- tau_V
        
        #Estimateurs courants:
        #-- tau
        tau_EMV_hat <- tau
        
        #-- probabilités de succès
        p0_esti <- p0_EMV
        p1_esti <- p1_EMV
        
      }
    }
  } 
  
  
  #Règle de décision pour la fraude:
  if( abs(p1_esti - p0_esti) > tresh){
    
    fraude = TRUE
  }
  
  res = tibble(tau_estim = tau_EMV_hat, tau_logvraiss = tau_V_max, 
               p0_estim =  p0_esti, p1_estim =  p1_esti,
               Fraude = fraude)
  
  return(res)
}

#' tau_EMV_K_unif
#'
#'
#' @description Detects a fraud in the given data serie using the MLE estimator for the time of fraud.
#' The considered values of frud events are drawn from a uniform distribution 
#'
#' @param data with rows being vectors of Bernoilli data points
#' @param tresh the treshold=abs(\hat{p_0}-\hat{p_1}) that is used to make the decision of whether the sample is a fraud or not.
#' @param K the number of treshold values to skip in order to test 2000/K points instead of 2000. 
#' 
#' @return A tibble summarizing the results of the algorithm 
#' 
#' @export
#'
#' @examples
#' data <- constuct_data2(n = 2000, nsimu = 1)$data
#' 
#' tau_EMV_K_unif(data)
#' 
tau_EMV_K_unif <- function(data, tresh = 0.2, K = 100){
  
  fraude = FALSE
  
  X <- data
  n <- length(X)
  
  tau_EMV_hat <- 0
  tau_V_max <- -3000
  p0_esti <- 0
  p1_esti <- 0
  
  grid <- runif(round(2000/K), min = 1, max = 1999)
  #Pour chaque position de tau possible, on itère:
  for(tau in grid){
    
    #Calcul de l'estimateur du maximum de vraissemblance de p0 et p1
    p0_EMV = sum(X[1:tau])/tau
    p1_EMV = sum(X[(tau+1):n])/(n-tau)
    
    #Calcul de la vraissemblance pour chaque tau
    tau_V = tau * log(1-p0_EMV) + (n-tau) * log(1-p1_EMV) + log(p0_EMV/(1-p0_EMV)) * sum(X[1:tau]) + log(p1_EMV/(1-p1_EMV)) * sum(X[(tau+1):n])
    
    
    
    #On ne garde que si la vraissemblance calculée pour le tau courant est supérieure à celle du précédent
    if(!(is.na(tau_V))){
      
      
      if(tau_V >= tau_V_max){
        
        
        #Vraissemblance maximale courrante
        tau_V_max <- tau_V
        
        #Estimateurs courants:
        #-- tau
        tau_EMV_hat <- tau
        
        #-- probabilités de succès
        p0_esti <- p0_EMV
        p1_esti <- p1_EMV
        
      }
    }
  } 
  
  
  #Règle de décision pour la fraude:
  if( abs(p1_esti - p0_esti) > tresh){
    
    fraude = TRUE
  }
  
  res = tibble(tau_estim = tau_EMV_hat, tau_logvraiss = tau_V_max, 
               p0_estim =  p0_esti, p1_estim =  p1_esti,
               Fraude = fraude)
  
  return(res)
}

#' tau_EMV_K_norm
#'
#'
#' @description Detects a fraud in the given data serie using the MLE estimator for the time of fraud.
#' The considered values of frud events are drawn from a normal distribution with mean=1000 and standard deviation=500
#'
#' @param data with rows being vectors of Bernoilli data points
#' @param tresh the treshold=abs(\hat{p_0}-\hat{p_1}) that is used to make the decision of whether the sample is a fraud or not.
#' @param K the number of treshold values to skip in order to test 2000/K points instead of 2000. 
#' 
#' @return A tibble summarizing the results of the algorithm 
#' 
#' @export
#'
#' @examples
#' data <- constuct_data2(n = 2000, nsimu = 1)$data
#' 
#' tau_EMV_K_norm(data)
#' 
tau_EMV_K_norm <- function(data, tresh = 0.2, K = 100){
  
  fraude = FALSE
  
  X <- data
  n <- length(X)
  
  tau_EMV_hat <- 0
  tau_V_max <- -3000
  p0_esti <- 0
  p1_esti <- 0
  
  grid <- round(rnorm(round(2000/K), mean = 1000, sd = 500))
  #Pour chaque position de tau possible, on itère:
  for(tau in grid){
    
    #Calcul de l'estimateur du maximum de vraissemblance de p0 et p1
    p0_EMV = sum(X[1:tau])/tau
    p1_EMV = sum(X[(tau+1):n])/(n-tau)
    
    #Calcul de la vraissemblance pour chaque tau
    tau_V = tau * log(1-p0_EMV) + (n-tau) * log(1-p1_EMV) + log(p0_EMV/(1-p0_EMV)) * sum(X[1:tau]) + log(p1_EMV/(1-p1_EMV)) * sum(X[(tau+1):n])
    
    
    
    #On ne garde que si la vraissemblance calculée pour le tau courant est supérieure à celle du précédent
    if(!(is.na(tau_V))){
      
      
      if(tau_V >= tau_V_max){
        
        
        #Vraissemblance maximale courrante
        tau_V_max <- tau_V
        
        #Estimateurs courants:
        #-- tau
        tau_EMV_hat <- tau
        
        #-- probabilités de succès
        p0_esti <- p0_EMV
        p1_esti <- p1_EMV
        
      }
    }
  } 
  
  
  #Règle de décision pour la fraude:
  if( abs(p1_esti - p0_esti) > tresh){
    
    fraude = TRUE
  }
  
  res = tibble(tau_estim = tau_EMV_hat, tau_logvraiss = tau_V_max, 
               p0_estim =  p0_esti, p1_estim =  p1_esti,
               Fraude = fraude)
  
  return(res)
}

#' tau_EMV
#'
#'
#' @description Detects a fraud in the given data serie using the MLE estimator for the time of fraud.
#' 
#'
#' @param data with rows being vectors of Bernoilli data points
#' @param tresh the treshold=abs(\hat{p_0}-\hat{p_1}) that is used to make the decision of whether the sample is a fraud or not.
#'  
#' 
#' @return A tibble summarizing the results of the algorithm 
#' 
#' @export
#'
#' @examples
#' data <- constuct_data2(n = 2000, nsimu = 1)$data
#' 
#' tau_EMV(data)
#' 
tau_EMV <- function(data, tresh = 0.2, K=100){
  
  fraude = FALSE
  
  X <- data
  n <- length(X)
  
  tau_V_max <- -3000
  p0_esti <- 0
  p1_esti <- 0
  
  #Pour chaque position de tau possible, on itère:
  for(tau in (1:(n-1))){
    
    #Calcul de l'estimateur du maximum de vraissemblance de p0 et p1
    p0_EMV = sum(X[1:tau])/tau
    p1_EMV = sum(X[(tau+1):n])/(n-tau)
    
    #Calcul de la vraissemblance pour chaque tau
    tau_V = tau * log(1-p0_EMV) + (n-tau) * log(1-p1_EMV) + log(p0_EMV/(1-p0_EMV)) * sum(X[1:tau]) + log(p1_EMV/(1-p1_EMV)) * sum(X[(tau+1):n])
    
    
    
    #On ne garde que si la vraissemblance calculée pour le tau courant est supérieure à celle du précédent
    if(!(is.na(tau_V))){
      
      
      if(tau_V >= tau_V_max){
        
        
        #Vraissemblance maximale courrante
        tau_V_max <- tau_V
        
        #Estimateurs courants:
        #-- tau
        tau_EMV_hat <- tau
        
        #-- probabilités de succès
        p0_esti <- p0_EMV
        p1_esti <- p1_EMV
        
      }
    }
  } 
  
  
  #Règle de décision pour la fraude:
  if( abs(p1_esti - p0_esti) > tresh){
    
    fraude = TRUE
  }
  
  res = tibble(tau_estim = tau_EMV_hat, tau_logvraiss = tau_V_max, 
               p0_estim =  p0_esti, p1_estim =  p1_esti,
               Fraude = fraude)
  
  return(res)
}
