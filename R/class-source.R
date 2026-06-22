#' Abstract snapshot source
#'
#' Subclass and implement `list_snapshots()`, `read_file()`, `list_tree()`.
#' @export
SnapshotSource <- R6::R6Class(
  "SnapshotSource",
  public = list(
    #' @field name Human-readable instance name.
    name = NULL,

    #' @description Create a source.
    #' @param name Instance name.
    initialize = function(name = NULL) {
      self$name <- name %||% class(self)[[1]]
    },

    #' @description List snapshots for a path.
    #' @param path Absolute file path.
    #' @return tibble(id, label, time).
    list_snapshots = function(path) cli::cli_abort("abstract: implement list_snapshots()"),

    #' @description Read file bytes at a snapshot.
    #' @param path Absolute file path.
    #' @param id Snapshot id from `list_snapshots()`.
    #' @return raw vector.
    read_file = function(path, id) cli::cli_abort("abstract: implement read_file()"),

    #' @description List directory entries at a snapshot.
    #' @param path Absolute directory path.
    #' @param id Optional snapshot id.
    #' @return tibble(path, type).
    list_tree = function(path, id = NULL) cli::cli_abort("abstract: implement list_tree()"),

    #' @description Classify bytes as "text", "image", or "binary".
    #' @param bytes raw vector.
    #' @param path file path (for extension hints).
    classify = function(bytes, path) {
      ext <- tolower(tools::file_ext(path))
      image_exts <- c("png", "jpg", "jpeg", "gif", "bmp", "webp", "svg")
      if (ext %in% image_exts) return("image")
      if (is_probably_text(bytes)) return("text")
      "binary"
    }
  )
)
