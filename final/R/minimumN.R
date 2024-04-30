#' Takes either one (x1) or two (x2) samples from preliminary data and returns minimum sample size for the t test of the null hypothesis that either mean(x1) = 0 or mean(x1) = mean(x2) with 80% power at alpha = 0.05
#'
#' @param x1 numerical vector
#' @param x2 (optional) numerical vector
#' @return minimum sample size
#' @importFrom pwr pwr.t2n.test
#' @examples
#' minimumN(x1 = c(1, 2, 3, 4, 5), x2 = c(2, 3, 4, 5, 6))
#' @export
minimumN <- function(x1, x2 = NULL) {
  if (is.null(x2)) {
    n = pwr.t.test(d = mean(x1)/stats::sd(x1), sig.level = 0.05, power = 0.8, alternative = "two.sided")$n
  } else {
    n = pwr.t2n.test(d = abs(mean(x1)-mean(x2))/sqrt((stats::sd(x1)^2 + stats::sd(x2)^2)), sig.level = 0.05, power = 0.8,  alternative = "two.sided")$n
  }
  return(ceiling(n))
}
