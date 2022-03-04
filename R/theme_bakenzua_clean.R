#' theme_bakenzua_clean
#'
#' ggplot defaults
#'
#' @return
#' @export
#'
#' @examples
#' ggplot2::theme_set(theme_bakenzua_clean())
theme_bakenzua_clean <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        size = 22,
        face = "bold",
        family = 'sans'
      ),
      axis.title = element_text(
        size = 18,
        face = "bold",
        family = 'sans'
      ),
      axis.title.y = element_text(margin = margin(r=10, l=-10)),
      axis.title.y = element_text(margin = margin(t=10, b=-10)),
      axis.title = element_text(
        size = 18,
        face = "bold",
        family = 'sans'
      ),
      axis.text = element_text(
        size = 12,
        family = 'sans'
      ),
      strip.text = element_text(face = "bold", size = rel(1), hjust = 0),
      strip.background = element_rect(fill = "grey80", color = NA),
      legend.title = element_text(
        size = 16,
        face = "bold",
        family = 'sans'
      ),
      legend.text = element_text(
        size = 14,
        family = 'sans'
      )
    )
}
