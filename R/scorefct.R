#' prec_score
#'
#'
#' @description computes the precision score of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the precision score of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' prec_score(data)
prec_score <- function(mat)
{
  tp <- mat[2,2]
  fp <- mat[2,1]
  
  return(tp/(tp+fp))
}

#' rec_score
#'
#'
#' @description computes the recall score of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the recall score of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' rec_score(data)

rec_score <- function(mat)
{
  tn <- mat[1,1]
  tp <- mat[2,2]
  
  return(tp/(tp+tn))
}

#' tpr_score
#'
#'
#' @description computes the true positive rate score of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the true positive rate score of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' tpr_score(data)
tpr_score <- function(mat){
  tp <- mat[2,2]
  fn <- mat[1,2] 
  
  return(tp/(tp+fn))
}

#' sensi_score
#'
#'
#' @description computes the sensibility score of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the true positive rate score of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' sensi_score(data)
sensi_score <- function(mat)
{
  tp <- mat[2,2]
  fn <- mat[1,2]
  
  return(tp/(tp+fn))
}

#' specif_score
#'
#'
#' @description computes the specitficity score of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the specitficity score of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' specif_score(data)
specif_score <- function(mat)
{
  tn <- mat[1,1]
  fp <- mat[2,1]
  
  
  return(tn/(tn+fp))
}

#' f1_score
#'
#'
#' @description computes the f1 score of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the f1 score of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' f1_score(data)
f1_score <- function(mat)
{
  precision <- prec_score(mat)
  recall <- rec_score(mat)
  
  
  return(2*(precision*recall)/(precision+recall))
}

#' acc_score
#'
#'
#' @description computes the accuracy score of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the accuracy score of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' acc_score(data)
acc_score <- function(mat)
{
  tn <- mat[1,1]
  fn <- mat[1,2]
  fp <- mat[2,1]
  tp <- mat[2,2]
  
  
  return((tn+tp)/(tn+fn+fp+tp))
}

#' tss_score
#'
#'
#' @description computes the True skill statistic of the given confusion matrix
#'
#' @param mat the confusion matrix
#'
#' @return the True skill statistic of the confusion matrix
#'
#' @examples
#' data <- matrix(c(15,2 , 5, 6))
#' 
#' tss_score(data)
tss_score <- function(mat)
{
  sensi <- sensi_score(mat)
  speci <- specif_score(mat)
  
  
  return(sensi + speci - 1)
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

#' compute_mf
#'
#'
#' @description computes the a confusio matrix from two vectors: one of labels and one of predictions, and computes a score
#' 
#' @param v_ver the labels
#' @param v_est the predictions
#' @param score the score to compute. Must be in: c("f1", "accuracy", "tp_rate","specificite", "sensibilite", "tss", "precision", "recall")
#' 
#' 
#' @return a list with the confusion matrix and the computed score
#'
#' @examples
#' data <- constuct_data2(n = 200, nsimu = 2)
#' labels <- data$data[1]
#' predictions <- data$data[2]
#' 
#' compute_mf(data)$Confusion
#' compute_mf(data)$score
compute_mf <- function(v_ver,v_est, score = "f1")
{
  
  # Initialisation de la matrice de confusion
  confusionmatrix <- matrix(data = c(0,0,0,0), nrow=2,ncol=2)
  
  colnames(confusionmatrix) <- c("-", "+")
  rownames(confusionmatrix) <- c("-", "+")
  
  ###########
  # TN # FN #
  ###########
  # FP # TP #
  ###########
  
  confusionmatrix[1,1] <- sum((v_ver == FALSE) & (v_est == FALSE)) #vrais negatfs
  confusionmatrix[1,2] <- sum((v_ver == TRUE) & (v_est == FALSE)) #faux négatifs
  confusionmatrix[2,1] <- sum((v_ver == FALSE) & (v_est == TRUE)) #faux positifs
  confusionmatrix[2,2] <- sum((v_ver == TRUE) & (v_est == TRUE)) #vrais positifs
  
  
  
  allowed.score <- c("f1", "accuracy", "tp_rate","specificite", "sensibilite", "tss", "precision", "recall")
  if(!score %in% allowed.score){stop('type must be one of: ', paste(allowed.score, collapse=", "))}
  
  if (score == "f1") {my_score <- f1_score(confusionmatrix)}
  else if (score == "accuracy") {my_score <- acc_score(confusionmatrix)}
  else if (score == "tp_rate") {my_score <- tpr_score(confusionmatrix)}
  else if (score == "specificite") {my_score <- specif_score(confusionmatrix)}
  else if (score == "sensibilite") {my_score <- sensi_score(confusionmatrix)}
  else if (score == "tss") {my_score <- tss_score(confusionmatrix)}
  else if (score == "precision") {my_score <- prec_score(confusionmatrix)}
  else if (score == "recall") {my_score <- rec_score(confusionmatrix)}
  
  
  return(list(Confusion = confusionmatrix, score = my_score))
  
}
