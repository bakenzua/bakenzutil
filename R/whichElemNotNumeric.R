#' whichElemNotNumeric
#'
#' whichElemNotNumeric returns the indices of a vector that cannot be cast as numeric types.
#' That is elements of a vector that would raise 'NAs introduced by coercion' when as.numeric() applied to vector.
#' Warnings are supressed.
#'
#' @param x A vector of non numeric values
#'
#' @return A vector of indices
#' @export
#'
#' @examples
#'
#' whichElemNotNumeric(c(1,2,3,NA_real_,'fred'))
#' [1] 4 5
whichElemNotNumeric <- function(x) {
  # detect
  which(is.na(suppressWarnings(as.numeric(as.character(x)))))
}
