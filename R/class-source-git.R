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

    #' @description List directory entries at a snapshot.
    #' @param path Absolute file or directory path within the repository, or NULL for the repo root.
    #' @param id Optional commit SHA; defaults to HEAD.
    #' @return tibble(path, type).
    list_tree = function(path, id = NULL) {
      ref  <- id %||% "HEAD"
      path <- path %||% self$repo
      rel  <- if (fs::is_dir(path)) fs::path_rel(path, self$repo)
              else fs::path_rel(fs::path_dir(path), self$repo)
      args <- c("ls-tree", "-r", "--name-only", ref)
      if (!identical(as.character(rel), ".")) args <- c(args, as.character(rel))
      out <- tryCatch(git_run(self$repo, args), error = function(e) character())
      out <- out[nzchar(out)]
      if (length(out) == 0) return(tibble::tibble(path = character(), type = character()))
      tibble::tibble(
        path = as.character(fs::path(self$repo, out)),
        type = "file"
      )
    }
  )
)
