#' make2part_SQL_query_with_list a SQL query helper
#'
#' make2part_SQL_query_with_list builds a query from 2 SQL fragments and a list of ids.
#' The first part of an ICIP SQL query is concatenated with a comma
#' separated list of ids and terminated with the second SQL fragment. 
#' 
#' NOTE: Would probably just do this with glue now
#'
#' @export
#'
#' @param sql_frags Vector of 2 SQL fragments
#' @param ids Vector of IDs to comma separate and concatenate
#' @param quotes Quote each id, default TRUE
#'
#' @return full_query
#'
make2part_SQL_query_with_list <- function(sql_frags, ids, quotes=TRUE) {

  if(quotes) {
    full_query <- paste0(
      sql_frags[1],
      "'",
      paste0(ids, collapse = "', '"),
      "'",
      sql_frags[2]
    )
  } else {
    full_query <- paste0(
      sql_frags[1],
      paste0(ids, collapse = ", "),
      sql_frags[2]
    )
  }
  return(stringr::str_squish(full_query))
}
