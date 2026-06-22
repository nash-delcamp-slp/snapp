#' Build a merged, time-sorted timeline for a path across sources
#' @param path Absolute file path (or NULL).
#' @param sources List of SnapshotSource instances.
#' @return tibble(source, id, label, time), sorted ascending by time.
#'   Emits a `snapp_source_error` warning per failing source and skips it.
#' @export
build_timeline <- function(path, sources) {
  empty <- tibble::tibble(source = character(), id = character(),
                          label = character(), time = as.POSIXct(character()))
  if (is.null(path) || length(sources) == 0) return(empty)

  parts <- lapply(sources, function(src) {
    tryCatch({
      snaps <- src$list_snapshots(path)
      if (nrow(snaps) == 0) return(NULL)
      tibble::tibble(source = src$name, id = snaps$id, label = snaps$label, time = snaps$time)
    }, error = function(e) {
      rlang::warn(
        sprintf("Source '%s' failed: %s", src$name, conditionMessage(e)),
        class = "snapp_source_error", source = src$name, parent = e
      )
      NULL
    })
  })

  out <- do.call(rbind, c(list(empty), parts))
  out <- out[order(out$time), , drop = FALSE]
  out <- out[!duplicated(out[c("source", "id")]), , drop = FALSE]
  tibble::as_tibble(out)
}
