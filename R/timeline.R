# Reserved sentinels for the synthetic "current on-disk version" timeline row.
# Package-internal; not exported.
LIVE_SOURCE <- "—current—"
LIVE_ID     <- "live"

#' Build a merged, time-sorted timeline for a path across sources
#' @param path Absolute file path (or NULL).
#' @param sources List of SnapshotSource instances.
#' @return tibble(source, id, label, time), sorted ascending by time. Includes a
#'   single synthetic `LIVE_SOURCE` row (the current on-disk version) when `path`
#'   is a regular file on disk. Emits a `snapp_source_error` warning per failing
#'   source and skips it.
#' @export
build_timeline <- function(path, sources) {
  empty <- tibble::tibble(source = character(), id = character(),
                          label = character(), time = as.POSIXct(character()))
  if (is.null(path)) return(empty)

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

  # Append the current on-disk version as one shared row, independent of sources.
  if (isTRUE(fs::is_file(path))) {
    live <- tibble::tibble(
      source = LIVE_SOURCE, id = LIVE_ID, label = "Current",
      time = as.POSIXct(file.info(path)$mtime, tz = "UTC"))
    out <- rbind(out, live)
  }

  out <- out[order(out$time), , drop = FALSE]
  out <- out[!duplicated(out[c("source", "id")]), , drop = FALSE]
  tibble::as_tibble(out)
}
