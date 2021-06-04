#' getSurvdiffPvalue gets p values from survdiff objects
#'
#' getSurvdiffPvalue gets p values from survival::survdiff() objects.
#' As per https://stat.ethz.ch/pipermail/r-help/2001-November/016742.html
#'
#' @param survdiffobj Object returned from survival::survdiff()
#'
#' @return p
#' @export
getSurvdiffPvalue <- function(survdiffobj)
{
  if( is.matrix(survdiffobj$obs))
    etmp <- apply(survdiffobj$exp, 1, sum)
  else
    etmp <- survdiffobj$exp
  df <- (sum(1 * (etmp > 0))) - 1
  pv <- 1 - pchisq(survdiffobj$chisq, df)
  pv
}
