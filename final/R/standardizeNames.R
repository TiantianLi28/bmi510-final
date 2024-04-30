#' Converts the variables in a tibble data to small camel case
#'
#' @param data A tibble
#' @return A tibble with variables converted to small camel case
#' @importFrom janitor make_clean_names
#' @importFrom dplyr rename_with
#' @examples
#' data = data.frame(Item = c('Apple','oranges','pears'), price = c(1,2,3))
#' standardizeNames(data)
#' @export
standardizeNames <- function(data) {
  clean_names = make_clean_names(names(data), case = "small_camel")
  data = rename_with(data, ~clean_names)
  return(data)
}
