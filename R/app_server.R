#' The application server-side
#' @param input,output,session Internal parameters for `{shiny}`.
#' @noRd
app_server <- function(input, output, session) {
  config_rv <- shiny::reactiveVal(read_config())
  launch_dir <- getwd()

  active_sources <- mod_sources_server(
    "sources", config_rv,
    discovered = shiny::reactive({
      if (!isTRUE(config_rv()$settings$auto_discover)) return(list())
      p <- selected_path() %||% launch_dir
      discover_sources(p)
    })
  )

  selected_path <- mod_file_browser_server("browser", active_sources)

  # Build the timeline, converting source-error warnings to notifications.
  timeline <- shiny::reactive({
    withCallingHandlers(
      build_timeline(selected_path(), active_sources()),
      snapp_source_error = function(w) {
        shiny::showNotification(conditionMessage(w), type = "warning")
        rlang::cnd_muffle(w)
      }
    )
  })

  mod_carousel_server("carousel", timeline, selected_path, active_sources,
                      default_view = shiny::reactive(config_rv()$settings$default_view))
  mod_settings_server("settings", config_rv)
}
