#' theme_bakenzua_clean
#'
#' Some personally appealing ggplot defaults
#'
#' @return theme_bakenzua_clean A ggplot theme
#' @export
#'
#' @examples
#' ggplot2::theme_set(theme_bakenzua_clean())
theme_bakenzua_clean <- function(...) {

  font <- "sans"

  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(size = 0.5),
      plot.title = element_text(
        color = '#222222',
        size = rel(2),
        face = "bold",
        family = font
      ),
      plot.subtitle = element_text(
        color = '#222222',
        size = rel(1.2),
        family = font
      ),
      axis.title = element_text(
        color = '#222222',
        size = rel(1.2),
        face = "bold",
        family = font
      ),
      axis.title.y = element_text(margin = margin(r=20, l=10)),
      axis.title.x = element_text(margin = margin(t=20, b=10)),
      # axis.line.x = element_line(size = 1),
      axis.text = element_text(
        color = '#222222',
        size = rel(1),
        family = font
      ),
      strip.text = element_text(
        color = '#222222',
        face = "bold",
        size = rel(1),
        hjust = 0
      ),
      strip.background = element_rect(fill = "grey80", color = NA),
      legend.title = element_text(
        color = '#222222',
        size = rel(1.6),
        face = "bold",
        family = font
      ),
      legend.text = element_text(
        color = '#222222',
        size = rel(1.2),
        family = font
      ),
      # Additional settings passed to theme()
      ...
    )

}
