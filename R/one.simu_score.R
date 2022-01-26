#' one.simu_score_ocOP
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
#' one.simu_score_ocOP(2, 1000)
one.simu_score_ocOP <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  ocOP <- onechangeOP(data)$tau
  data_pred <- y_predict(data, ocOP)
  score <- nid_score(data, data_pred)
  return(score)
}

#' one.simu_score_ocPELT
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
#' one.simu_score_ocPELT(2, 1000)
one.simu_score_ocPELT <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  ocPELT <- onechangePELT(data)$tau
  data_pred <- y_predict(data, ocPELT)
  score <- nid_score(data, data_pred)
  return(score)
}


#' one.simu_score_OP
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
#' one.simu_score_OP(2, 1000)
one.simu_score_OP <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  OP <- myOP(data)$changepoints
  data_pred <- y_predict(data, OP)
  score <- nid_score(data, data_pred)
  return(score)
}


#' one.simu_score_PELT
#'
#'
#' @description one simulation function for PELT
#'
#' @param K number of changepoint
#' @param n size of each "segment" of the data
#'
#' @return the time used to realise one simulation
#' @export
#'
#' @examples
#' one.simu_score_PELT(2, 1000)
one.simu_score_PELT <- function(K, n)
{
  data <- dataSeries(K, n = 100, probs = sample(1:9,K)/10)
  PELT <- myPELT(data)$changepoints
  data_pred <- y_predict(data, PELT)
  score <- nid_score(data, data_pred)
  return(score)
}