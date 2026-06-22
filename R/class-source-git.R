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
    repo = NULL,
    initialize = function(repo, name = NULL) {
      super$initialize(name)
      self$repo <- fs::path_abs(repo)
    },
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
    read_file = function(path, id) {
      rel <- fs::path_rel(path, self$repo)
      git_read_raw(self$repo, paste0(id, ":", rel))
    },
    list_tree = function(path, id = NULL) {
      ref <- id %||% "HEAD"
      rel <- if (fs::is_dir(path)) fs::path_rel(path, self$repo) else fs::path_rel(fs::path_dir(path), self$repo)
      arg <- if (identical(as.character(rel), ".")) ref else paste0(ref, ":", rel)
      # git ls-tree default output: <mode> <type> <sha>\t<name>
      # --format was added in git 2.36; fall back to parsing default output for compatibility.
      out <- tryCatch(git_run(self$repo, c("ls-tree", arg)),
                      error = function(e) character())
      out <- out[nzchar(out)]
      if (length(out) == 0) return(tibble::tibble(path = character(), type = character()))
      # Split on tab to get left part (mode type sha) and right part (filename)
      parts <- strsplit(out, "\t", fixed = TRUE)
      names_vec <- vapply(parts, `[[`, character(1), 2)
      left_vec  <- vapply(parts, `[[`, character(1), 1)
      # type is the second space-separated token in the left part
      types_raw <- vapply(strsplit(left_vec, " "), `[[`, character(1), 2)
      base <- if (identical(as.character(rel), ".")) self$repo else fs::path(self$repo, rel)
      tibble::tibble(
        path = as.character(fs::path(base, names_vec)),
        type = ifelse(types_raw == "tree", "dir", "file")
      )
    }
  )
)
