#' theme_bakenzua_clean
#'
#' ggplot defaults
#'
#' @return
#' @export
#'
#' @examples
#' ggplot2::theme_set(theme_bakenzua_clean())
theme_bakenzua_clean <- function(...) {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(size = 0.5),
      plot.title = element_text(
        size = rel(2),
        face = "bold",
        family = 'sans'
      ),
      plot.subtitle = element_text(
        size = rel(1.2),
        family = 'sans'
      ),
      axis.title = element_text(
        size = rel(1.2),
        face = "bold",
        family = 'sans'
      ),
      axis.title.y = element_text(margin = margin(r=20, l=10)),
      axis.title.x = element_text(margin = margin(t=20, b=10)),
      # axis.line.x = element_line(size = 1),
      axis.text = element_text(
        size = rel(1),
        family = 'sans'
      ),
      strip.text = element_text(face = "bold", size = rel(1), hjust = 0),
      strip.background = element_rect(fill = "grey80", color = NA),
      legend.title = element_text(
        size = rel(1.6),
        face = "bold",
        family = 'sans'
      ),
      legend.text = element_text(
        size = rel(1.2),
        family = 'sans'
      ),
      # Additional settings passed to theme()
      ...
    )

}
