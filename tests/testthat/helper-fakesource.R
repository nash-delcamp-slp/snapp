# In-memory source for deterministic tests.
# data: named list keyed by absolute path; each value is a list of snapshots,
# each snapshot a list(id, label, time, content=<raw|character>).
FakeSource <- R6::R6Class(
  "FakeSource",
  inherit = SnapshotSource,
  public = list(
    data = NULL,
    fail = FALSE,
    initialize = function(name = "fake", data = list(), fail = FALSE) {
      super$initialize(name)
      self$data <- data
      self$fail <- fail
    },
    list_snapshots = function(path) {
      if (self$fail) stop("fake failure")
      snaps <- self$data[[path]] %||% list()
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
    list_tree = function(path, id = NULL) {
      tibble::tibble(path = names(self$data), type = "file")
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
