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
    dataset_root = NULL,
    snapshot_glob = NULL,
    time_from = NULL,
    initialize = function(dataset_root, snapshot_glob, time_from = NULL, name = NULL) {
      super$initialize(name)
      self$dataset_root  <- fs::path_abs(dataset_root)
      self$snapshot_glob <- snapshot_glob
      self$time_from     <- time_from
    },
    snapshot_dirs = function() Sys.glob(self$snapshot_glob),
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
    read_file = function(path, id) {
      rel <- fs::path_rel(path, self$dataset_root)
      target <- fs::path(id, rel)
      readBin(target, "raw", n = file.info(target)$size %||% 0)
    },
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
