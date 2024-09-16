#' median_iqr_string
#'
#' returns a formatted string detailing median and IQR
#'
#' @param x A vector of numeric values to be evaluated
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @param probs numeric vector of probabilities with values in [0,1], default 0.25 and 0.75, only first two probs will be reported as range
#' @param digits numeric number of digits for rounding, default=2
#'
#' @return result_string
#' @export
#'
median_iqr_string <- function(x, na.rm=T, probs=c(0.25, 0.75), digits=2) {

  result_string <- paste0(
    round(median(x, na.rm = na.rm), digits = digits),
    ' [',
    round(quantile(x, probs=probs[1]), digits = digits),
    ', ',
    round(quantile(x, probs=probs[2]), digits = digits),
    ']'
  )
  return(result_string)
}
