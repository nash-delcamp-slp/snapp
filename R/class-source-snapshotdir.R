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

    #' @field time_from Optional override: when set, snapshot timestamps are parsed from
    #'   the snapshot directory NAME (`list(regex, format)`); otherwise the file's own
    #'   mtime is used.
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

    #' @description Timestamp for a snapshot copy of a file.
    #' @param dir Absolute snapshot directory path.
    #' @param file Absolute path to the file inside that snapshot.
    #' @return POSIXct. Parsed from the snapshot dir NAME if `time_from` is set
    #'   (and matches); otherwise the file's own mtime.
    snapshot_time = function(dir, file) {
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
      as.POSIXct(file.info(file)$mtime, tz = "UTC")
    },

    #' @description List the distinct versions of a file across snapshots.
    #' @param path Absolute file path within the dataset root.
    #' @return tibble(id, label, time), one row per distinct version (snapshots
    #'   sharing a timestamp are collapsed), ordered ascending by time. `id` is the
    #'   absolute snapshot directory path of the earliest snapshot holding that version.
    list_snapshots = function(path) {
      empty <- tibble::tibble(id = character(), label = character(),
                              time = as.POSIXct(character()))
      rel <- as.character(fs::path_rel(path, self$dataset_root))
      if (identical(rel, ".")) return(empty)
      rel_depth <- length(fs::path_split(rel)[[1]])
      matched <- Sys.glob(file.path(self$snapshot_glob, rel))   # full-path glob (reliable)
      if (length(matched) == 0) return(empty)
      snapdirs <- vapply(matched, function(m) {
        d <- m
        for (k in seq_len(rel_depth)) d <- fs::path_dir(d)
        as.character(fs::path_abs(d))
      }, character(1), USE.NAMES = FALSE)
      times <- do.call(c, Map(function(d, f) self$snapshot_time(d, f), snapdirs, matched))
      o <- order(times)
      snapdirs <- snapdirs[o]
      times <- times[o]
      keep <- !duplicated(times)                                 # collapse identical versions
      tibble::tibble(id = snapdirs[keep], label = basename(snapdirs[keep]), time = times[keep])
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
