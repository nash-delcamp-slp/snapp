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

#' List immediate children of a live directory, fault-tolerantly.
#' Hidden entries (dotfiles, incl. .zfs) are excluded. Unreadable dir -> empty.
#' @noRd
safe_list_children <- function(dir) {
  empty <- tibble::tibble(name = character(), path = character(), type = character())
  entries <- tryCatch(fs::dir_ls(dir, recurse = FALSE, all = FALSE),
                      error = function(e) NULL)
  if (is.null(entries) || length(entries) == 0) return(empty)
  isdir <- tryCatch(fs::is_dir(entries), error = function(e) rep(FALSE, length(entries)))
  tibble::tibble(
    name = as.character(fs::path_file(entries)),
    path = as.character(entries),
    type = ifelse(isdir, "dir", "file")
  )
}

#' Is `ancestor` an ancestor of or equal to `path`? (normalized string compare)
#' @noRd
is_ancestor_or_equal <- function(ancestor, path) {
  a <- as.character(fs::path_abs(ancestor)); p <- as.character(fs::path_abs(path))
  identical(a, p) || startsWith(p, paste0(a, "/"))
}
