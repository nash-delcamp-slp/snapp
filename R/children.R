#' Merge immediate children across sources at a path
#' @param sources List of SnapshotSource instances; lists children from sources
#'   whose root contains `path`.
#' @param path Absolute directory path.
#' @return tibble(name, path, type), deduped by path, dirs first then files (alpha).
#' @export
merge_children <- function(sources, path) {
  empty <- tibble::tibble(name = character(), path = character(), type = character())
  if (length(sources) == 0) return(empty)
  sources <- Filter(function(s) {
    r <- tryCatch(s$root(), error = function(e) NULL)
    !is.null(r) && is_ancestor_or_equal(r, path)
  }, sources)
  if (length(sources) == 0) return(empty)
  parts <- lapply(sources, function(src) {
    tryCatch(src$list_children(path), error = function(e) {
      rlang::warn(sprintf("Source '%s' listing failed: %s", src$name, conditionMessage(e)),
                  class = "snapp_source_error", source = src$name, parent = e)
      NULL
    })
  })
  out <- do.call(rbind, c(list(empty), parts))
  out <- out[!duplicated(out$path), , drop = FALSE]
  out <- out[order(out$type != "dir", tolower(out$name)), , drop = FALSE]
  tibble::as_tibble(out)
}

#' Bounded recursive file search by name substring (case-insensitive)
#' @param sources List of SnapshotSource instances.
#' @param start_dir Directory to search under.
#' @param query Substring to match against file names.
#' @param max_results,max_nodes Caps to keep the search bounded.
#' @return tibble(name, path, type) of matching files (type == "file").
#' @export
find_files <- function(sources, start_dir, query, max_results = 300L, max_nodes = 5000L) {
  empty <- tibble::tibble(name = character(), path = character(), type = character())
  if (!nzchar(query %||% "") || length(sources) == 0) return(empty)
  q <- tolower(query)
  results <- empty; queue <- as.character(start_dir); visited <- character(0)
  while (length(queue) && nrow(results) < max_results && length(visited) < max_nodes) {
    d <- queue[[1]]; queue <- queue[-1]
    if (d %in% visited) next
    visited <- c(visited, d)
    rel <- Filter(function(s) is_ancestor_or_equal(s$root(), d), sources)
    kids <- merge_children(rel, d)
    if (nrow(kids) == 0) next
    files <- kids[kids$type == "file", , drop = FALSE]
    if (nrow(files)) {
      hit <- files[grepl(q, tolower(files$name), fixed = TRUE), , drop = FALSE]
      if (nrow(hit)) results <- rbind(results, hit)
    }
    queue <- c(queue, kids$path[kids$type == "dir"])
  }
  tibble::as_tibble(utils::head(results, max_results))
}
