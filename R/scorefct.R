prec_score <- function(mat)
{
  ###########
  # TN # FN #
  ###########
  # FP # TP #
  ###########
  
  tp <- mat[2,2]
  fp <- mat[2,1]
  
  return(tp/(tp+fp))
}


rec_score <- function(mat)
{
  ###########
  # TN # FN #
  ###########
  # FP # TP #
  ###########
  
  tn <- mat[1,1]
  tp <- mat[2,2]
  
  return(tp/(tp+tn))
}


f1_score <- function(mat)
{
  ###########
  # TN # FN #
  ###########
  # FP # TP #
  ###########
  
  precision <- prec_score(mat)
  recall <- rec_score(mat)
  
  return(2*(precision*recall)/(precision+recall))
}


acc_score <- function(mat)
{
  ###########
  # TN # FN #
  ###########
  # FP # TP #
  ###########
  
  tn <- mat[1,1]
  fn <- mat[1,2]
  fp <- mat[2,1]
  tp <- mat[2,2]
  
  return((tn+tp)/(tn+fn+fp+tp))
}





tss_score <- function(mat)
{
  precision <- prec_score(mat)
  recall <- rec_score(mat)
  
  return(precision + recall - 1)
}


y_predict <- function(y, tau)
{
  n <- length(y)
  y_pred <- rep(y[1],n)
  for(k in tau)
  {
    print(tau)
    y_pred[k:n] <- y_pred[(k-1)] + 1
  }
  
  y_pred <- y_pred %% 2
  return(y_pred)
}


nid_score <- function(y, y_pred)
{
  scores <- clustComp(y, y_pred)
  return(scores$NID)
}


compute_mf <- function(v_ver,v_est, score = "f1")
{
  allowed.score <- c("f1", "accuracy", "precision", "recall", "tss", "nid")
  if(!score %in% allowed.score){stop('type must be one of: ', paste(allowed.score, collapse=", "))}
  
  if (score == "f1") {score_f <- f1_score}
  else if (score == "accuracy") {score_f <- acc_Score}
  else if (score == "precision") {score_f <- prec_Score}
  else if (score == "recall") {score_f <- rec_Score}
  else if (score == "tss") {score_f <- tss_score}
  else if (score == "nid") {score_f <- nid_score}
  
  # Initialisation de la matrice de confusion
  confusionmatrix <- matrix(data = c(0,0,0,0), nrow=2,ncol=2)
  colnames(confusionmatrix) <- c("-", "+")
  rownames(confusionmatrix) <- c("-", "+")
  
  # Construction de la matrice de confusion
  tab1 <- table(v_est %in% v_ver) #ici on construit le tableau des elements estimés vs les éléments de vérité, si on a un élément de tauhat dans tau, alors c'est un vrai positif, sinon c'est un faux positif
  tab2 <- table(v_ver %in% v_est) # ici on fait pareil pour différencier les elements de vérité vs les elements estimés
  
  
  ###########
  # TN # FN #
  ###########
  # FP # TP #
  ###########
  
  
  confusionmatrix[1,1] <- tab1[2] #vrais négatifs
  confusionmatrix[1,2] <- tab1[1] #faux négatifs
  confusionmatrix[2,1] <- tab2[1] #faux positifs
  confusionmatrix[2,2] <- tab2[2] #vrais positifs
  
  
  if(score == "nid") {my_score <- nid_score(v_ver, v_est)} else {my_score <- score_f(confusionmatrix)}

  
  return(list(Confusion = confusionmatrix, score = my_score))
  
}