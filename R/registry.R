# Internal registry: type name -> list(new = fn(params), detect = fn(path)|NULL)
.snapp_registry <- new.env(parent = emptyenv())

#' Register a snapshot source type
#' @param type Type name used in config (e.g. "git").
#' @param new Function taking a params list, returning a SnapshotSource instance.
#' @param detect Optional function(path) returning a list of candidate configs,
#'   each `list(name, type, params)`.
#' @export
register_source_type <- function(type, new, detect = NULL) {
  stopifnot(is.character(type), length(type) == 1, is.function(new))
  assign(type, list(new = new, detect = detect), envir = .snapp_registry)
  invisible(type)
}

#' Names of registered source types
#' @export
source_types <- function() sort(ls(.snapp_registry))

#' Instantiate a source by type
#' @param type Registered type name.
#' @param params Named list of constructor params.
#' @export
new_source <- function(type, params = list()) {
  if (!exists(type, envir = .snapp_registry, inherits = FALSE)) {
    cli::cli_abort("Unknown source type {.val {type}}.")
  }
  get(type, envir = .snapp_registry)$new(params)
}

#' Discover applicable sources for a path
#' @param path Absolute path being explored.
#' @return list of candidate configs `list(name, type, params)`, deduped.
#' @export
discover_sources <- function(path) {
  out <- list()
  for (type in source_types()) {
    detect <- get(type, envir = .snapp_registry)$detect
    if (is.null(detect)) next
    cand <- tryCatch(detect(path), error = function(e) list())
    out <- c(out, cand)
  }
  keys <- vapply(out, function(c) paste(c$type, digest_params(c$params)), character(1))
  out[!duplicated(keys)]
}

#' @noRd
digest_params <- function(params) paste(names(params), unlist(params), sep = "=", collapse = ";")
