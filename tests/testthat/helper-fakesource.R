# In-memory source for deterministic tests.
# data: named list keyed by absolute path; each value is a list of snapshots,
# each snapshot a list(id, label, time, content=<raw|character>).
# When data values are bare scalars (e.g. 1), list_children derives structure from keys.
FakeSource <- R6::R6Class(
  "FakeSource",
  inherit = SnapshotSource,
  lock_objects = FALSE,
  public = list(
    data = NULL,
    fail = FALSE,
    .root = NULL,
    initialize = function(name = "fake", data = list(), fail = FALSE, root = NULL) {
      super$initialize(name)
      self$data <- data
      self$fail <- fail
      self$.root <- root %||% {
        keys <- names(data)
        if (length(keys) == 0) "/" else {
          # Use the dirname of the first key as root, or "/" if bare
          d <- dirname(keys[[1]])
          if (nzchar(d) && d != ".") d else "/"
        }
      }
    },
    list_snapshots = function(path) {
      if (self$fail) stop("fake failure")
      snaps <- self$data[[path]] %||% list()
      if (!is.list(snaps) || (length(snaps) > 0 && !is.list(snaps[[1]]))) {
        # data value is a bare scalar, not a snapshot list
        return(tibble::tibble(id = character(), label = character(),
                              time = as.POSIXct(character())))
      }
      tibble::tibble(
        id    = vapply(snaps, function(s) s$id, character(1)),
        label = vapply(snaps, function(s) s$label, character(1)),
        time  = as.POSIXct(vapply(snaps, function(s) as.numeric(s$time), numeric(1)),
                           origin = "1970-01-01", tz = "UTC")
      )
    },
    read_file = function(path, id) {
      snaps <- self$data[[path]] %||% list()
      hit <- Filter(function(s) s$id == id, snaps)[[1]]
      if (is.character(hit$content)) charToRaw(paste(hit$content, collapse = "\n")) else hit$content
    },
    root = function() self$.root,
    list_children = function(path = NULL) {
      if (self$fail) stop("fake failure")
      dir <- path %||% self$.root
      dir <- as.character(dir)
      keys <- names(self$data)
      empty <- tibble::tibble(name = character(), path = character(), type = character())
      if (length(keys) == 0) return(empty)
      # Normalise dir to have trailing "/" for prefix matching
      dir_slash <- if (endsWith(dir, "/")) dir else paste0(dir, "/")
      # Children are keys that start with dir_slash; take only the next path segment
      immediate <- list()
      for (k in keys) {
        if (!startsWith(k, dir_slash)) next
        rest <- substring(k, nchar(dir_slash) + 1)
        seg <- strsplit(rest, "/", fixed = TRUE)[[1]][[1]]
        child_path <- paste0(dir_slash, seg)
        # Determine if this child is a dir (key continues past this segment)
        is_dir <- nchar(rest) > nchar(seg)
        type <- if (is_dir) "dir" else "file"
        if (!child_path %in% names(immediate)) {
          immediate[[child_path]] <- list(name = seg, path = child_path, type = type)
        } else if (is_dir) {
          # If we already have it as file but now know it's a dir, upgrade
          immediate[[child_path]]$type <- "dir"
        }
      }
      if (length(immediate) == 0) return(empty)
      out <- do.call(rbind, lapply(immediate, function(x) {
        tibble::tibble(name = x$name, path = x$path, type = x$type)
      }))
      tibble::as_tibble(out)
    }
  )
)

# Convenience constructor for a single-file history.
fake_history <- function(path, snaps, name = "fake", fail = FALSE) {
  FakeSource$new(name = name, data = stats::setNames(list(snaps), path), fail = fail)
}

# Snapshot & restore the registry around a test.
local_registry <- function(env = parent.frame()) {
  old <- as.list(.snapp_registry)
  withr::defer({
    rm(list = ls(.snapp_registry), envir = .snapp_registry)
    for (nm in names(old)) assign(nm, old[[nm]], envir = .snapp_registry)
  }, envir = env)
  rm(list = ls(.snapp_registry), envir = .snapp_registry)
}
