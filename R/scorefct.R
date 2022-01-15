scorefct <- function(tau,tauhat, score = "f1")
{
  allowed.score <- c("f1", "accuracy", "precision", "recall", "tss")
  if(!score %in% allowed.score){stop('type must be one of: ', paste(allowed.score, collapse=", "))}
  
  if (score == "f1") {score_f <- f1_score}
  else if (score == "accuracy") {score_f <- acc_Score}
  else if (score == "precision") {score_f <- prec_Score}
  else if (score == "recall") {score_f <- rec_Score}
  else if (score == "tss") {score_f <- tss_score}
  
  # Initialisation de la matrice de confusion
  confusionmatrix <- matrix(data = NULL, nrow=2,ncol=2)
  colnames(confusionmatrix) <- c("-", "+")
  rownames(confusionmatrix) <- c("-", "+")
  
  # Construction de la matrice de confusion
  tab1 <- table(tauhat %in% tau) #ici on construit le tableau des elements de tau estimé vs les éléments de tau, si on a un élément de tauhat dans tau, alors c'est un vrai positif, sinon c'est un faux positif
  tab2 <- table(tau %in% tauhat) # ici on fait pareil pour différencier les elements de tau vs les elements de tau estimé
  
  
  ###########
  # TN # FN #
  ###########
  # FP # TP #
  ###########
  
  
  confusionmatrix[1,1] <- tab1[2] #vrais négatifs
  confusionmatrix[1,2] <- tab1[1] #faux négatifs
  confusionmatrix[2,1] <- tab2[1] #faux positifs
  confusionmatrix[2,2] <- tab2[2] #vrais positifs
  
  my_score <- score_f(confusionmatrix)
  
  return(list(Confusion = confusionmatrix, score = my_score))
  
}