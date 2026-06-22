#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Heuristic: are these bytes probably text?
#'
#' Samples the first 8000 bytes; presence of a NUL byte means binary.
#' @noRd
is_probably_text <- function(bytes) {
  if (length(bytes) == 0) return(TRUE)
  chunk <- utils::head(bytes, 8000L)
  !any(chunk == as.raw(0L))
}
