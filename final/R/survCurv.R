#' Takes a numerical vector status and a numerical vector time, and calculate and plot the survival curve S(t)
#'
#' @param status numerical vector
#' @param time numerical vector
#' @return None
#' @examples
#' survCurv(status, time)
#' @export
survCurv = function(status, time){
  times = sort(unique(time))
  cumu_probs = numeric(length(times))
  for (i in 1:length(cumu_probs)){
    cumu_probs[i] = sum(status == 1 & time > times[i])/length(status)
  }
  plot(times, cumu_probs, main = "Survival Curve", xlab = "Time", ylab = "Probability of Survival")
}
