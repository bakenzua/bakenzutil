#' check_icnarc_cmp_format
#'
#' Validate formatting of ICNARC CMP codes in a data frame column.
#'
#' This function checks that every value in a specified column of a data frame
#' follows the ICNARC CMP code format: five integers separated by dots, where
#' the first integer is 1 or 2 (for example, "1.2.7.11.6").
#'
#' @param df A data.frame or tibble containing the column to check.
#' @param column_name A string giving the name of the column in `df` to validate.
#' @param strict Logical scalar; if `TRUE` the function will throw an error when
#'   any value does not match the expected format. If `FALSE` (default) the
#'   function returns a logical vector indicating which rows are correctly
#'   formatted.
#'
#' @details
#' - The check treats missing (NA) values as invalid (i.e., they yield `FALSE`).
#' - Values are coerced to character for matching; non-character inputs (numeric,
#'   factor) are supported via coercion.
#' - The regular expression used enforces five dot-separated integer components,
#'   and restricts the first component to 1 or 2.
#'
#' @return
#' If `strict = FALSE` the function returns a logical vector with length equal
#' to `nrow(df)`, where `TRUE` indicates the value in `column_name` matches the
#' ICNARC CMP pattern. If `strict = TRUE` the function returns invisibly the
#' same logical vector on success, but will raise an error listing invalid row
#' indices when any values fail the check.
#'
#' @examples
#' df <- data.frame(code = c("1.2.7.11.6", "2.10.3.5.1", "bad", NA), stringsAsFactors = FALSE)
#' check_icnarc_cmp_format(df, "code")
#' try(check_icnarc_cmp_format(df, "code", strict = TRUE))
#'
#' @export
check_icnarc_cmp_format <- function(df, column_name, strict = FALSE) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble")
  }
  if (!is.character(column_name) || length(column_name) != 1) {
    stop("`column_name` must be a single string")
  }
  if (!column_name %in% names(df)) {
    stop("Column '", column_name, "' not found in dataframe")
  }

  # Define expected pattern: five integers separated by dots, first integer 1 or 2
  pattern <- "^[12]\\.\\d+\\.\\d+\\.\\d+\\.\\d+$"

  # Coerce to character and run the pattern match
  values <- as.character(df[[column_name]])
  is_valid <- grepl(pattern, values)

  # Treat NA as invalid
  is_valid[is.na(is_valid)] <- FALSE

  if (isTRUE(strict) && !all(is_valid)) {
    invalid_rows <- which(!is_valid)
    stop("Invalid code format in rows: ", paste(invalid_rows, collapse = ", "),
         "\nExpected format: five integers separated by dots, first integer must be 1 or 2 (e.g., '1.2.7.11.6')")
  }

  invisible(is_valid)
}
