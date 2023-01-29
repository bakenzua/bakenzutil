#' egof_table
#'
#' A function to ape the "egof table" of STATA
#' Pass a fitted glm object and specify the quantiles to group rpedicted probabilities
#' by, and returns tabulated observed and expected outcome counts.
#' Halfway to a Hosmer-Lemeshow test
#'
#' Requires dplyr
#'
#' @param glm_fit Fiited GLM model
#' @param n_quantiles Number of quantiles to group by
#'
#' @return data frame of observed and expected outcome counts
#' @export
egof_table <- function(glm_fit, n_quantiles = 10) {
  df <- glm_fit$data

  df$predict_probs <- predict(glm_fit, type = "response")

  df$predict_probs_deciles <- cut(df$predict_probs, n_quantiles, labels = 1:n_quantiles)

  df |>
    group_by(predict_probs_deciles) |>
    summarise(
      # group = glm_p12q2_predict_probs_deciles,
      ng = n(),
      pi_g_bar = mean(predict_probs),
      observed_y0 = sum(ht==0),
      expected_y0 = (1 - pi_g_bar) * ng,
      observed_y1 = sum(ht==1),
      expected_y1 = pi_g_bar * ng
    )
}
