#' prettyprint_est_ci
#'
#' Returns a string formatting an estimate and confidence interval bounds
#'
#' @param est Estimate
#' @param ll  Confidence interval lower limit
#' @param ul  Confidence interval upper limit
#' @param digits Digits to round to
#' @param ci_pc Confidence interval percentage (default: 95)
#'
#' @returns A string
#' @export
#'
#' @examples
prettyprint_est_ci <- function(est, ll, ul, digits=2, ci_pc="95") {
  glue::glue(
    "{s1} ({ci}%CI: {s2}, {s3})",
    s1 = round(est, digits = digits),
    ci = ci_pc,
    s2 = round(ll, digits = digits),
    s3 = round(ul, digits = digits)
  )
}


