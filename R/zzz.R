#' pasteX
#'
#' paste a vector to a string (for dplyr summarize chars).
#' @param vec a char vector.
#' @param collapse which collapse is to used in `paste`.
#' @return a string
pasteX <- function(vec, collapse = ";") {
    paste(vec, collapse = collapse)
}
