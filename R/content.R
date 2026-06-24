#' Fetch and classify content for a timeline entry
#'
#' @param path Absolute file path.
#' @param entry A list/row with `source` (name) and `id`. When `source` is the
#'   reserved `LIVE_SOURCE`, bytes are read directly from the live on-disk `path`.
#' @param sources List of active SnapshotSource instances.
#' @return list(type, bytes, lines, hash).
#' @export
fetch_content <- function(path, entry, sources) {
  if (identical(entry$source, LIVE_SOURCE)) {
    bytes <- readBin(path, "raw", n = file.info(path)$size %||% 0)
    type  <- SnapshotSource$new()$classify(bytes, path)
  } else {
    src <- Find(function(s) identical(s$name, entry$source), sources)
    if (is.null(src)) cli::cli_abort("No active source named {.val {entry$source}}.")
    bytes <- src$read_file(path, entry$id)
    type  <- src$classify(bytes, path)
  }
  lines <- if (identical(type, "text")) {
    strsplit(rawToChar(bytes), "\n", fixed = TRUE)[[1]]
  } else NULL
  list(type = type, bytes = bytes, lines = lines, hash = content_hash(bytes))
}
