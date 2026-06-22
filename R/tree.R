#' Merge directory trees across sources into one deduped listing
#' @param sources List of SnapshotSource instances.
#' @param path Optional directory path; NULL lists each source's root.
#' @return tibble(path, type), deduped by path.
#' @export
merge_tree <- function(sources, path = NULL) {
  empty <- tibble::tibble(path = character(), type = character())
  parts <- lapply(sources, function(src) {
    tryCatch(src$list_tree(path), error = function(e) {
      rlang::warn(sprintf("Source '%s' tree failed: %s", src$name, conditionMessage(e)),
                  class = "snapp_source_error", source = src$name, parent = e)
      NULL
    })
  })
  out <- do.call(rbind, c(list(empty), parts))
  out <- out[!duplicated(out$path), , drop = FALSE]
  out <- out[order(out$path), , drop = FALSE]
  tibble::as_tibble(out)
}
