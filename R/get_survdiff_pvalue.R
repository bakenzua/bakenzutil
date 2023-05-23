#' get_survdiff_pvalue gets p values from survdiff objects
#'
#' get_survdiff_pvalue gets p values from survival::survdiff() objects.
#' As per https://stat.ethz.ch/pipermail/r-help/2001-November/016742.html
#' 
#' If things are not readily available in R it is always good to pause and 
#' reflect if there might be a good reason.
#' --Brian D. Ripley (about how to get t statistics for arima())
#' R-help March 2013
#'
#' @param survdiffobj Object returned from survival::survdiff()
#'
#' @return p
#' @export
get_survdiff_pvalue <- function(survdiffobj)
{
  if( is.matrix(survdiffobj$obs))
    etmp <- apply(survdiffobj$exp, 1, sum)
  else
    etmp <- survdiffobj$exp
  df <- (sum(1 * (etmp > 0))) - 1
  pv <- 1 - pchisq(survdiffobj$chisq, df)
  pv
}