#' Path to the user config file
#' @export
config_path <- function() {
  getOption("snapp.config_path",
            Sys.getenv("SNAPP_CONFIG", file.path(path.expand("~"), ".snapp", "config.yml")))
}

#' Default configuration
#' @export
default_config <- function() {
  list(
    settings = list(default_view = "side-by-side", auto_discover = TRUE),
    sources  = list()
  )
}

#' Validate a configuration list; abort with a classed error if invalid
#' @param cfg Config list.
#' @export
validate_config <- function(cfg) {
  if (!is.list(cfg) || is.null(cfg$settings) || is.null(cfg$sources)) {
    cli::cli_abort(c("Invalid config.", "x" = "Must have {.field settings} and {.field sources}."),
                   class = "snapp_config_error")
  }
  view <- cfg$settings$default_view
  if (!isTRUE(view %in% c("side-by-side", "unified"))) {
    cli::cli_abort("Invalid {.field default_view}: must be \"side-by-side\" or \"unified\".",
                   class = "snapp_config_error")
  }
  for (s in cfg$sources) {
    if (is.null(s$name) || is.null(s$type)) {
      cli::cli_abort("Each source needs a {.field name} and {.field type}.",
                     class = "snapp_config_error")
    }
  }
  invisible(cfg)
}

#' Read config (creating a validated default if the file is absent)
#' @param path Config file path.
#' @export
read_config <- function(path = config_path()) {
  if (!file.exists(path)) {
    cfg <- default_config()
    write_config(cfg, path)
    return(cfg)
  }
  cfg <- yaml::read_yaml(path)
  validate_config(cfg)
}

#' Write config to disk (validating first)
#' @param cfg Config list.
#' @param path Config file path.
#' @export
write_config <- function(cfg, path = config_path()) {
  validate_config(cfg)
  fs::dir_create(fs::path_dir(path))
  yaml::write_yaml(cfg, path)
  invisible(path)
}
