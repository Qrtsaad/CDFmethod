#Choix du tau de rupture par maximum de vraissemblance
tau_EMV <- function(data, tresh = 0.2){
  
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
        tau_EMV <- tau
        
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
  
  res = tibble(tau_estim = tau_EMV, tau_logvraiss = tau_V_max, 
               p0_estim =  p0_esti, p1_estim =  p1_esti,
               Fraude = fraude)
  
  return(res)
}
