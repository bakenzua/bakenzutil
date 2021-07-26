#' dateifyFilename
#'
#' dateifyFilename adds underscore + date(as YYYYMMDD) before the extension of a filename
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
dateifyFilename <- function(filename){
  splits <- stringr::str_split(filename, '\\.')
  paste0(
    splits[[1]][1],
    '_',
    stringr::str_replace_all(as.Date(now()), '-', ''),
    '.',
    splits[[1]][2]
  )
}
