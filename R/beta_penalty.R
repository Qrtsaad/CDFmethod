#' Best Beta
#'
#'
#' @description Best parameter for penalty
#'
#' @param data vector of data points
#'
#' @return best penalty value
#' @export
#'
#' @examples
#' best_beta(c(0,1,1,2,2,3))
best_beta <- function(data)
{
  n <- length(data)
  return (2*var(data)*log(n))
}


#' BIC Beta
#'
#'
#' @description BIC penalty
#'
#' @param data vector of data points
#'
#' @return BIC penalty value
#' @export
#'
#' @examples
#' BIC_beta(c(0,1,1,2,2,3))
BIC_beta <- function(data)
{
  n <- length(data)
  return(2*log(n))
}
