#' check_icnarc_cmp_format
#'
#' Checks the correct formatting of column of ICNARC CMP codes in a dataframe.
#'
#' @param df
#' @param column_name
#' @param strict If TRUE, will stop, otherwise returns a boolean vector indicating correct formatting
#'
#' @returns A boolean vector indicating correct formatting
#' @export
#'
#' @examples
check_icnarc_cmp_format <- function(df, column_name, strict = FALSE) {
  # Check if column exists
  if (!column_name %in% names(df)) {
    stop("Column '", column_name, "' not found in dataframe")
  }

  # Define the expected pattern: five integers separated by dots
  pattern <-  "^[12]\\.\\d+\\.\\d+\\.\\d+\\.\\d+$"

  # Check each value against the pattern
  is_valid <- grepl(pattern, df[[column_name]])

  if (strict && !all(is_valid)) {
    invalid_rows <- which(!is_valid)
    stop("Invalid code format in rows: ", paste(invalid_rows, collapse = ", "),
         "\nExpected format: five integers separated by dots, first digit must be 1 or 2 (e.g., '1.2.7.11.6')")
  }

  return(is_valid)
}
