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

  fmt_str <- glue::glue("%.{d}f", d=digits)
  glue::glue(
    "{s1} ({ci}{s2}, {s3})",
    s1 = sprintf(fmt_str, est),
    ci = dplyr::if_else(
      ci_pc == "",
      "",
      glue::glue("{ci_pc}%CI:")
    ),
    s2 = sprintf(fmt_str, ll),
    s3 = sprintf(fmt_str, ul)
  )
}


