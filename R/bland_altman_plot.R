#' Bland–Altman (mean-difference) plot
#'
#' Create a Bland–Altman plot comparing two numeric measurement variables.
#'
#' The function computes the per-row mean and difference between two measurements
#' and produces a ggplot2 scatterplot of difference versus mean. It adds a horizontal
#' line for the mean difference and the limits of agreement (mean ± 1.96 * SD of differences),
#' with textual annotations for those values.
#'
#' @param data A data.frame or tibble containing the measurement columns.
#' @param var_a Unquoted column name for the first measurement (uses tidy evaluation).
#' @param var_b Unquoted column name for the second measurement (uses tidy evaluation).
#' @param plot_title Character. Plot title. Default: \"Bland-Altman Plot of var_a vs. var_b\".
#' @param xlab Character. X-axis label. Default: \"Mean (var_a - var_b)/2\".
#' @param ylab Character. Y-axis label. Default: \"Difference in measurements (var_a - var_b)\".
#' @param mean_colour Character. Colour used for the mean-difference line. Default: \"blue\".
#' @param sd_colour Character. Colour used for the limits of agreement lines. Default: \"red\".
#'
#' @return A ggplot2 object (difference vs mean) with annotated mean and ±1.96 SD lines.
#'
#' @details
#' - The mean for each pair is calculated as (var_a + var_b) / 2.
#' - The difference is calculated as var_a - var_b.
#' - Limits of agreement are computed as mean(diff) ± 1.96 * sd(diff).
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' bland_altman_plot(iris, Sepal.Length, Sepal.Width)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @export
bland_altman_plot <- function(
  data,
  var_a,
  var_b,
  plot_title="Bland-Altman Plot of var_a vs. var_b",
  xlab = "Mean (var_a - var_b)/2",
  ylab = "Difference in measurements (var_a - var_b)",
  mean_colour ="blue",
  sd_colour = "red"
) {
  # calclulate some data points
  data <- data |>
    mutate(
      mean = ({{ var_a }} + {{ var_b }}) / 2,
      diff = {{ var_a }} - {{ var_b }}
    )
  mean_diff <- mean(data$diff)
  sd_diff <- sd(data$diff)
  min_mean <- min(data$mean)
  max_mean <- max(data$mean)
  ul <- mean_diff + (1.96 * sd_diff)
  ll <- mean_diff - (1.96 * sd_diff)
  x_labels <- min_mean - 0.1 * (max_mean -min_mean)
  x_lim <- c(min_mean - 0.2 * (max_mean - min_mean), max_mean + 0.1 * (max_mean - min_mean))

  # construct plot
  plot <- ggplot(data, aes(mean, diff)) +
    geom_hline(yintercept = mean_diff, colour=mean_colour) +
    annotate("text", x= x_labels, y = mean_diff, label=glue::glue("Mean = {d}", d=round(mean_diff,2)), vjust=1.5, colour=mean_colour) +
    geom_hline(yintercept = ul, colour=sd_colour) +
    annotate("text", x= x_labels, y = ul, label=glue::glue("+1.96 SD = {d}", d=round(ul, 2)), vjust=-0.5, colour=sd_colour) +
    geom_hline(yintercept = ll, colour=sd_colour) +
    annotate("text", x= x_labels, y = ll, label=glue::glue("-1.96 SD = {d}", d=round(ll, 2)), vjust=1.5, colour=sd_colour) +
    geom_point(shape=1, size=4) +
    coord_cartesian(xlim = x_lim) +
    labs(
      x=xlab, y=ylab, title = plot_title
    )
}