#' Find a dataset root containing a .zfs/snapshot dir by ascending
#' @param path File or dir path.
#' @return Dataset root path, or NULL.
#' @export
find_zfs_root <- function(path) {
  dir <- if (fs::is_dir(path)) path else fs::path_dir(path)
  dir <- fs::path_abs(dir)
  repeat {
    if (fs::dir_exists(fs::path(dir, ".zfs", "snapshot"))) return(as.character(dir))
    parent <- fs::path_dir(dir)
    if (identical(as.character(parent), as.character(dir))) return(NULL)
    dir <- parent
  }
}

#' Generalized filesystem-snapshot source
#'
#' Reads point-in-time copies from a set of snapshot directories (e.g. ZFS
#' `.zfs/snapshot/<stamp>/`). `id` is the absolute snapshot directory path.
#' @export
SnapshotDirSource <- R6::R6Class(
  "SnapshotDirSource",
  inherit = SnapshotSource,
  public = list(
    #' @field dataset_root Absolute path to the live dataset root.
    dataset_root = NULL,

    #' @field snapshot_glob Glob pattern that expands to snapshot directories.
    snapshot_glob = NULL,

    #' @field time_from Optional named list with `regex` and optional `format` (scalar
    #'   or character vector of formats tried in order) for parsing snapshot timestamps
    #'   from directory names. When NULL (the default), timing uses the file's own mtime
    #'   inside the snapshot directory.
    time_from = NULL,

    #' @description Create a SnapshotDirSource.
    #' @param dataset_root Path to the live dataset root.
    #' @param snapshot_glob Glob pattern that expands to snapshot directories.
    #' @param time_from Named list with `regex`/`format` for timestamp parsing, or NULL.
    #' @param name Optional human-readable instance name.
    initialize = function(dataset_root, snapshot_glob, time_from = NULL, name = NULL) {
      super$initialize(name)
      self$dataset_root  <- fs::path_abs(dataset_root)
      self$snapshot_glob <- snapshot_glob
      self$time_from     <- time_from
    },

    #' @description Expand the snapshot glob to a character vector of snapshot directories.
    #' @return Character vector of absolute snapshot directory paths.
    snapshot_dirs = function() Sys.glob(self$snapshot_glob),

    #' @description Extract a POSIXct timestamp from a snapshot directory path.
    #' @param dir Absolute path to a snapshot directory.
    #' @param file Optional absolute path to the specific file inside the snapshot;
    #'   when provided (and `time_from` is not set), its mtime is used instead of
    #'   the directory mtime.
    #' @return POSIXct timestamp.
    snapshot_time = function(dir, file = NULL) {
      tf <- self$time_from
      if (!is.null(tf) && !is.null(tf$regex)) {
        m <- regmatches(basename(dir), regexec(tf$regex, basename(dir)))[[1]]
        if (length(m) >= 2) {
          for (fmt in (tf$format %||% "%Y-%m-%dT%H:%M:%S")) {
            t <- as.POSIXct(m[[2]], format = fmt, tz = "UTC")
            if (!is.na(t)) return(t)
          }
        }
      }
      target <- file %||% dir
      as.POSIXct(file.info(target)$mtime, tz = "UTC")
    },

    #' @description List snapshots containing a given file path.
    #' @param path Absolute file path within the dataset root.
    #' @return tibble(id, label, time).
    list_snapshots = function(path) {
      rel  <- fs::path_rel(path, self$dataset_root)
      dirs <- self$snapshot_dirs()
      hits <- unlist(Filter(function(d) fs::file_exists(fs::path(d, rel)), dirs))
      empty <- tibble::tibble(id = character(), label = character(),
                              time = as.POSIXct(character()))
      if (length(hits) == 0) return(empty)
      times <- do.call(c, lapply(hits, function(d) self$snapshot_time(d, fs::path(d, rel))))
      o <- order(times); hits <- hits[o]; times <- times[o]
      keep <- !duplicated(times)            # collapse unchanged (identical-mtime) versions
      hits <- hits[keep]; times <- times[keep]
      tibble::tibble(id = as.character(fs::path_abs(hits)), label = basename(hits), time = times)
    },

    #' @description Read raw file bytes from a snapshot.
    #' @param path Absolute file path within the dataset root.
    #' @param id Absolute snapshot directory path from `list_snapshots()`.
    #' @return raw vector of file bytes.
    read_file = function(path, id) {
      rel <- fs::path_rel(path, self$dataset_root)
      target <- fs::path(id, rel)
      readBin(target, "raw", n = file.info(target)$size %||% 0)
    },

    #' @description Root directory for navigation.
    root = function() as.character(self$dataset_root),

    #' @description List immediate children of a directory (one level).
    #' @param path Absolute directory path, or NULL for the source root.
    #' @return tibble(name, path, type) where type is "dir" or "file".
    list_children = function(path = NULL) {
      path <- path %||% self$dataset_root
      dir  <- if (fs::is_dir(path)) path else fs::path_dir(path)
      safe_list_children(dir)
    }
  )
)
