#' Takes data x and number of principle component npc and approximate x based on npc.
#'
#' @param x A dataframe
#' @param npc number of principle component
#' @return approximation
#' @export
pcApprox = function(x, npc){
  result = prcomp(x_centered, scale = TRUE)
  pc = result$x[,1:npc]
  approx = pc %*% t(result$rotation[, 1:npc])
  approx = approx * attr(result$scale, 'scaled:scale')+attr(result$mean, 'mean')
  return(approx)
}
