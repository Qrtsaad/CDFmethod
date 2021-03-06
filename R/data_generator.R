#' Data Series
#'
#'
#'@description Generating data from a bernoulli model = changepoint relative positions + parameters + type of cost
#'
#' @param nb the number of bernoulli distribution B(pi) segment
#' @param n number of data points for each bernoulli distribution B(pi)
#' @param probs a vector of probability for each bernoulli distribution B(pi)
#' 
#' @return data a vector of size nb*n generated by the chosen model
#' @export
#'
#' @examples
#' plot(dataSeries(), type = 'l')
dataSeries <- function(K = 5, n = 100, probs = sample(1:9,K)/10)
{
  if(length(probs) != K){stop('length of probabilities must be equal to the value nb')}
  
  data <- NULL
  for(k in 1:K)
  {
    data <- c(data, rbernoulli(n, p = probs[k]))
  }
    
  return(as.numeric(data))
}
