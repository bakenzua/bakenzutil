#' qqladder_df
#'
#' Returns long dataframe of transformed data ordered by transformation and value
#' As per stata qladder function
#'
#' @param data Vector of values for transformation
#'
#' @return Dataframe of transfromed data
#' @export
#'
#' @examples
#' df <- qqladder_df(rnorm(100, 10, 1))
qqladder_df <- function(data) {

  if(length(data[data<=0])) {
    stop('Stopped:  Negative values in data')
  }

  order <- c(
    "cubic", "square", "identity", "sqrt",
    "log", "1/sqrt", "inverse", "1/square",
    "1/cubic"
  )

  tibble(var = data) |>
    mutate(identity = var,
           cubic = identity^3,
           square = identity^2,
           sqrt = sqrt(identity),
           log = log(identity),
           `1/sqrt` = 1/.data$sqrt,
           inverse = 1/.data$identity,
           `1/square` = 1/.data$square,
           `1/cubic` = 1/.data$cubic
    ) |>
    select(-var) |>
    pivot_longer(
      cols = everything(),
      names_to = 'Transformation',
      values_to = 'value'
    ) |>
    # filter(!is.na(value)) |>
    mutate(
      Transformation = factor(
        Transformation,
        levels = order
      )
    ) |>
    arrange(Transformation, value)
}

#' qqladder
#'
#' @param data A vector of values for transformation  as per qqladder_df
#' and plotting qq plot for each transformation
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' qqladder(rnorm(100, 10, 1))
#'
qqladder <- function(data) {
  qqladder_df(data) |>
    ggplot(aes(sample=value)) +
    geom_qq(alpha = 0.3) +
    stat_qq_line() +
    facet_wrap(~Transformation, scales = "free") +
    labs(
      x = "Value",
      y = "Fraction"
    ) +
    theme_bw()
}
