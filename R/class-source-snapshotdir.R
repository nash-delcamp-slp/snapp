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

    #' @field time_from Named list with `regex` and optional `format` for parsing
    #'   snapshot timestamps from directory names; NULL to use mtime.
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
    #' @return POSIXct timestamp.
    snapshot_time = function(dir) {
      tf <- self$time_from
      if (!is.null(tf) && !is.null(tf$regex)) {
        m <- regmatches(basename(dir), regexec(tf$regex, basename(dir)))[[1]]
        if (length(m) >= 2) {
          t <- as.POSIXct(m[[2]], format = tf$format %||% "%Y-%m-%dT%H:%M:%S", tz = "UTC")
          if (!is.na(t)) return(t)
        }
      }
      as.POSIXct(file.info(dir)$mtime)
    },
    #' @description List snapshots containing a given file path.
    #' @param path Absolute file path within the dataset root.
    #' @return tibble(id, label, time).
    list_snapshots = function(path) {
      rel <- fs::path_rel(path, self$dataset_root)
      dirs <- self$snapshot_dirs()
      hits <- Filter(function(d) fs::file_exists(fs::path(d, rel)), dirs)
      if (length(hits) == 0) {
        return(tibble::tibble(id = character(), label = character(),
                              time = as.POSIXct(character())))
      }
      tibble::tibble(
        id = as.character(fs::path_abs(unlist(hits))),
        label = basename(unlist(hits)),
        time = do.call(c, lapply(hits, function(d) self$snapshot_time(d)))
      )
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

    #' @description List directory entries across snapshots.
    #' @param path Absolute file or directory path within the dataset root.
    #' @param id Optional snapshot directory path to restrict to; NULL scans all snapshots.
    #' @return tibble(path, type) with deduplicated live dataset paths.
    list_tree = function(path, id = NULL) {
      rel_dir <- if (fs::is_dir(path)) fs::path_rel(path, self$dataset_root)
                 else fs::path_rel(fs::path_dir(path), self$dataset_root)
      dirs <- if (is.null(id)) self$snapshot_dirs() else id
      rows <- lapply(dirs, function(d) {
        base <- fs::path(d, rel_dir)
        if (!fs::dir_exists(base)) return(NULL)
        entries <- fs::dir_ls(base)
        tibble::tibble(
          # map snapshot path back to the live dataset path
          path = as.character(fs::path(self$dataset_root, fs::path_rel(entries, d))),
          type = ifelse(fs::is_dir(entries), "dir", "file")
        )
      })
      out <- do.call(rbind, c(list(tibble::tibble(path = character(), type = character())), rows))
      out[!duplicated(out$path), , drop = FALSE]
    }
  )
)
