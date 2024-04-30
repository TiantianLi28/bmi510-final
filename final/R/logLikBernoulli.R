#' Takes a vector c and searches for the parameter p that maximizes the the log likelihood log(P(p|c)) using gridsearch with interval 0.001
#'
#' @param data A binary vector.
#' @return p that maximizes the log likelihood
#' @examples
#' logLikBernoulli(c(0,1,0,0,1))
#' @export
logLikBernoulli = function(data){
  prob = seq(0, 1, by = 0.001)
  max_lik = -Inf
  max_p = 0
  for (p in prob){
    lik = sum(data*log(p)+(1-data)*log(1-p))
    if (lik > max_lik) {
      max_lik = lik
      max_p = p
    }
  }
  return (max_p)
}
