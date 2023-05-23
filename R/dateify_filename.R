#' dateify_filename
#'
#' dateify_filename adds underscore + date(as YYYYMMDD) before the extension of a filename
#'
#' @param filename
#'
#' @return String of decorated filename
#' @export
#'
dateify_filename <- function(filename){
  splits <- stringr::str_split(filename, '\\.')
  paste0(
    splits[[1]][1],
    '_',
    stringr::str_replace_all(Sys.Date(), '-', ''),
    '.',
    splits[[1]][2]
  )
}
