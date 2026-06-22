.onLoad <- function(libname, pkgname) {
  register_builtin_sources()
}

#' @noRd
register_builtin_sources <- function() {
  register_source_type(
    "git",
    new = function(params) do.call(GitSource$new, params),
    detect = function(path) {
      repo <- find_git_root(path)
      if (is.null(repo)) return(list())
      list(list(name = paste0("git: ", basename(repo)), type = "git",
                params = list(repo = repo)))
    }
  )

  register_source_type(
    "zfs",
    new = function(params) {
      params$snapshot_glob <- params$snapshot_glob %||%
        as.character(fs::path(params$dataset_root, ".zfs", "snapshot", "*"))
      do.call(SnapshotDirSource$new, params)
    },
    detect = function(path) {
      root <- find_zfs_root(path)
      if (is.null(root)) return(list())
      list(list(name = paste0("zfs: ", root), type = "zfs",
                params = list(dataset_root = root)))
    }
  )
}
