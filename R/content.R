#' Fetch and classify content for a timeline entry
#' @param path Absolute file path.
#' @param entry A list/row with `source` (name) and `id`.
#' @param sources List of active SnapshotSource instances.
#' @return list(type, bytes, lines).
#' @export
fetch_content <- function(path, entry, sources) {
  src <- Find(function(s) identical(s$name, entry$source), sources)
  if (is.null(src)) cli::cli_abort("No active source named {.val {entry$source}}.")
  bytes <- src$read_file(path, entry$id)
  type  <- src$classify(bytes, path)
  lines <- if (identical(type, "text")) {
    strsplit(rawToChar(bytes), "\n", fixed = TRUE)[[1]]
  } else NULL
  list(type = type, bytes = bytes, lines = lines)
}
