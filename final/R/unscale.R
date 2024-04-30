#' Takes a vector that has been through the scale function in r and reverse the scaling, if any.
#'
#' @param x A vector that was passed through scale
#' @return unscaled vector
#' @examples
#' unscale(scale(c(1,2,3,4,5)))
#' @export
unscale = function(x) {
  m = attr(x, 'scaled:center')
  sd = attr(x, 'scaled:scale')
  unscaled = (x * sd) + m
  return(unscaled)
}
