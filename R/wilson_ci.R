#' wilson_ci Wilson confidence interval for Binomial proportion
#'
#' wilson_ci 
#'
#' @param events Number of outcomes(successes)
#' @param trials Total number of trials
#' @param alpha
#'
#' @return vector of lower and upper confidence interval
#' @export
#'
#' @examples
#'
#'     The core code for this function is from
#'     https://rpubs.com/brouwern/binomialCI2
#'     https://stats.stackexchange.com/questions/59733/can-agresti-coull-binomial#-confidence-intervals-be-negative
#'     and was written by https://stats.stackexchange.com/users/21054/coolserdash
#'
#'     for more info see
#'     wikipedia.org/wiki/Binomial_proportion_confidence_interval

wilson_ci <- function(events, # events = outcomes
                      trials, # number of individuals, test, etc
                      alpha = 0.05) {
    n <- trials
    x <- events
    p.hat <- x / n
    
    z_threshold_pos <- qnorm(1 - (alpha / 2))
    z_threshold_neg <- qnorm(alpha / 2)
    # Calculate upper and lower limit
    upper.lim <- (p.hat + (z_threshold_pos^2 / (2 * n)) + z_threshold_pos * sqrt(((p.hat * (1 - p.hat)) / n) + (z_threshold_pos^2 / (4 * n^2)))) / (1 + (z_threshold^2 / (n)))

    lower.lim <- (p.hat + (z_threshold_neg^2 / (2 * n)) + z_threshold_neg * sqrt(((p.hat * (1 - p.hat)) / n) + (z_threshold_neg^2 / (4 * n^2)))) / (1 + (z_threshold_neg^2 / (n)))


    # Modification for probabilities close to boundaries

    if ((n <= 50 & x %in% c(1, 2)) | (n >= 51 & n <= 100 & x %in% c(1:3))) {
        lower.lim <- 0.5 * qchisq(alpha, 2 * x) / n
    }

    if ((n <= 50 & x %in% c(n - 1, n - 2)) | (n >= 51 & n <= 100 & x %in% c(n - (1:3)))) {
        upper.lim <- 1 - 0.5 * qchisq(alpha, 2 * (n - x)) / n
    }

    out <- c(lower.lim, upper.lim)
    names(out) <- c("lower.CI", "upper.CI")
    return(out)

}