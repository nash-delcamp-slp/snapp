#' Settings modal trigger UI
#' @noRd
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("open"), "\u2699 Settings", class = "btn-sm btn-outline-secondary")
}

#' Settings server: edit config.yml via a modal
#' @param id Module id.
#' @param config_rv reactiveVal holding the config list.
#' @noRd
mod_settings_server <- function(id, config_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::observeEvent(input$open, {
      shiny::showModal(shiny::modalDialog(
        title = "Settings",
        shiny::selectInput(ns("default_view"), "Default view",
                           choices = c("side-by-side", "unified"),
                           selected = config_rv()$settings$default_view),
        shiny::checkboxInput(ns("auto_discover"), "Auto-discover sources",
                             value = isTRUE(config_rv()$settings$auto_discover)),
        shiny::tags$hr(),
        shiny::tags$label("Raw config.yml"),
        shiny::textAreaInput(ns("raw"), NULL, rows = 12,
                             value = yaml::as.yaml(config_rv())),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("save"), "Save"))
      ))
    })
    shiny::observeEvent(input$save, {
      cfg <- tryCatch(yaml::yaml.load(input$raw), error = function(e) NULL)
      if (is.null(cfg)) { shiny::showNotification("Invalid YAML", type = "error"); return() }
      cfg$settings$default_view  <- input$default_view
      cfg$settings$auto_discover <- input$auto_discover
      ok <- tryCatch({ write_config(cfg); TRUE },
                     error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); FALSE })
      if (ok) { config_rv(cfg); shiny::removeModal()
                shiny::showNotification("Settings saved", type = "message") }
    })
  })
}
