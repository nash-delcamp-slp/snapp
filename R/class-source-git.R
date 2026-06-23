#' @noRd
git_run <- function(repo, args) {
  out <- suppressWarnings(system2("git", c("-C", repo, args), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    cli::cli_abort("git {args[[1]]} failed: {paste(out, collapse = ' ')}")
  }
  out
}

#' @noRd
git_read_raw <- function(repo, spec) {
  tmp <- tempfile(); on.exit(unlink(tmp))
  status <- suppressWarnings(system2("git", c("-C", repo, "show", spec),
                                     stdout = tmp, stderr = FALSE))
  if (!is.null(status) && status != 0) cli::cli_abort("git show {spec} failed.")
  if (!file.exists(tmp)) return(raw(0))
  readBin(tmp, "raw", n = file.info(tmp)$size %||% 0)
}

#' Find the git repo root by ascending from a path
#' @param path A file or directory path.
#' @return Repo root path, or NULL.
#' @export
find_git_root <- function(path) {
  dir <- if (fs::is_dir(path)) path else fs::path_dir(path)
  dir <- fs::path_abs(dir)
  repeat {
    if (fs::dir_exists(fs::path(dir, ".git"))) return(as.character(dir))
    parent <- fs::path_dir(dir)
    if (identical(as.character(parent), as.character(dir))) return(NULL)
    dir <- parent
  }
}

#' Git snapshot source
#' @export
GitSource <- R6::R6Class(
  "GitSource",
  inherit = SnapshotSource,
  public = list(
    #' @field repo Absolute path to the git repository root.
    repo = NULL,

    #' @description Create a GitSource.
    #' @param repo Path to the git repository root.
    #' @param name Optional human-readable instance name.
    initialize = function(repo, name = NULL) {
      super$initialize(name)
      self$repo <- fs::path_abs(repo)
    },

    #' @description List snapshots (commits) for a file path.
    #' @param path Absolute file path within the repository.
    #' @return tibble(id, label, time).
    list_snapshots = function(path) {
      rel <- fs::path_rel(path, self$repo)
      out <- git_run(self$repo, c("log", "--follow",
                                  "--format=%H%x1f%ct%x1f%s", "--", rel))
      out <- out[nzchar(out)]
      if (length(out) == 0) {
        return(tibble::tibble(id = character(), label = character(),
                              time = as.POSIXct(character())))
      }
      m <- do.call(rbind, strsplit(out, "\x1f", fixed = TRUE))
      tibble::tibble(
        id = m[, 1],
        label = m[, 3],
        time = as.POSIXct(as.numeric(m[, 2]), origin = "1970-01-01", tz = "UTC")
      )
    },
    #' @description Read raw file bytes at a specific commit.
    #' @param path Absolute file path within the repository.
    #' @param id Commit SHA from `list_snapshots()`.
    #' @return raw vector of file bytes.
    read_file = function(path, id) {
      rel <- fs::path_rel(path, self$repo)
      git_read_raw(self$repo, paste0(id, ":", rel))
    },

    #' @description Root directory for navigation.
    root = function() as.character(self$repo),

    #' @description List immediate children of a directory (one level).
    #' @param path Absolute directory path, or NULL for the source root.
    #' @return tibble(name, path, type) where type is "dir" or "file".
    list_children = function(path = NULL) {
      path <- path %||% self$repo
      rel  <- if (fs::is_dir(path)) fs::path_rel(path, self$repo)
              else fs::path_rel(fs::path_dir(path), self$repo)
      arg  <- if (identical(as.character(rel), ".")) "HEAD" else paste0("HEAD:", as.character(rel))
      out  <- tryCatch(git_run(self$repo, c("ls-tree", arg)), error = function(e) character())
      out  <- out[nzchar(out)]
      empty <- tibble::tibble(name = character(), path = character(), type = character())
      if (length(out) == 0) return(empty)
      tabs <- strsplit(out, "\t", fixed = TRUE)          # "<mode> <type> <sha>\t<name>"
      nm   <- vapply(tabs, function(x) x[[2]], character(1))
      meta <- vapply(tabs, function(x) x[[1]], character(1))
      type <- ifelse(grepl(" tree ", meta, fixed = TRUE), "dir", "file")
      base <- if (identical(as.character(rel), ".")) self$repo else fs::path(self$repo, rel)
      tibble::tibble(name = nm, path = as.character(fs::path(base, nm)), type = type)
    }
  )
)
