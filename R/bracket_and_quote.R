#' bracket_and_quote
#'
#' bracket_and_quote takes a string vector, wraps each string in single quotes,
#' collapses all the strings into a comma separated version wrapped in () brackets.
#'
#' @param string_vec
#'
#' @return
#' @export
#'
#' @examples
bracket_and_quote <- function(string_vec) {
  glue::glue(
    "({all_strings})",
    all_strings=glue::glue_collapse(
      glue::glue("'{strings}'",
           strings = string_vec
      ),
      sep=", "
    )
  )
}
