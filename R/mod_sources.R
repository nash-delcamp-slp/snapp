#' Sources sidebar UI
#' @noRd
mod_sources_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("list")),
    shiny::actionButton(ns("add"), "\u2795 Add source", class = "btn-sm btn-outline-secondary")
  )
}

#' Sources server
#' @param id Module id.
#' @param config_rv reactiveVal holding the config list.
#' @param discovered reactive list of discovered candidate configs.
#' @return reactive list of enabled SnapshotSource instances.
#' @noRd
mod_sources_server <- function(id, config_rv, discovered = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    enabled <- shiny::reactiveValues()   # source key -> logical, session-persistent toggle state
    has_obs <- new.env(parent = emptyenv())  # source key -> TRUE once its sync observer exists

    instances <- shiny::reactive({
      cfg <- config_rv()
      conf <- lapply(cfg$sources, function(s) {
        inst <- tryCatch(new_source(s$type, s$params %||% list()), error = function(e) NULL)
        if (is.null(inst)) return(NULL)
        inst$name <- s$name
        list(inst = inst, key = s$name, saved = TRUE, default = isTRUE(s$enabled))
      })
      disc <- lapply(discovered(), function(s) {
        inst <- tryCatch(new_source(s$type, s$params %||% list()), error = function(e) NULL)
        if (is.null(inst)) return(NULL)
        inst$name <- s$name
        list(inst = inst, key = s$name, saved = FALSE, default = TRUE)   # discovered default ON
      })
      conf <- Filter(Negate(is.null), conf)
      disc <- Filter(Negate(is.null), disc)
      conf_keys <- vapply(conf, function(x) x$key, character(1))
      disc <- Filter(function(x) !(x$key %in% conf_keys), disc)          # configured wins on key clash
      c(conf, disc)
    })

    # Seed session toggle state (once per key, default-on) + one sync observer per key.
    shiny::observe({
      for (it in instances()) {
        key <- it$key
        if (is.null(enabled[[key]])) enabled[[key]] <- isTRUE(it$default)
        if (is.null(has_obs[[key]])) {
          has_obs[[key]] <- TRUE
          local({
            k <- key
            cbid <- paste0("en_", make.names(k))
            shiny::observeEvent(input[[cbid]], {
              enabled[[k]] <- isTRUE(input[[cbid]])
            }, ignoreNULL = FALSE, ignoreInit = TRUE)
          })
        }
      }
    })

    output$list <- shiny::renderUI({
      items <- instances()
      if (length(items) == 0) return(shiny::p(class = "text-muted", "No sources. Add or discover one."))
      lapply(items, function(it) {
        cb <- ns(paste0("en_", make.names(it$key)))
        val <- shiny::isolate(enabled[[it$key]])     # isolate: a re-render reads persisted state, never resets it
        if (is.null(val)) val <- isTRUE(it$default)
        shiny::div(class = "source-row",
          shiny::checkboxInput(cb, it$key, value = val),
          if (!it$saved) shiny::tags$span(class = "badge bg-info", "discovered"))
      })
    })

    # Add-source modal
    shiny::observeEvent(input$add, {
      shiny::showModal(shiny::modalDialog(
        title = "Add source",
        shiny::selectInput(ns("new_type"), "Type", choices = source_types()),
        shiny::textInput(ns("new_name"), "Name"),
        shiny::textAreaInput(ns("new_params"), "Params (yaml)", value = "repo: /path/to/repo"),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("save_new"), "Save"))
      ))
    })
    shiny::observeEvent(input$save_new, {
      params <- tryCatch(yaml::yaml.load(input$new_params), error = function(e) NULL)
      if (is.null(params)) {
        shiny::showNotification("Params must be valid, non-empty YAML (e.g. repo: /path/to/repo).",
                                type = "error")
        return()
      }
      cfg <- config_rv()
      cfg$sources <- c(cfg$sources, list(list(
        name = input$new_name, type = input$new_type, enabled = TRUE, params = params)))
      ok <- tryCatch({ write_config(cfg); TRUE },
                     error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); FALSE })
      if (ok) { config_rv(cfg); shiny::removeModal() }
    })

    # Enabled instances reactive: read the PERSISTENT store (not the raw input).
    shiny::reactive({
      items <- instances()
      keep <- Filter(function(it) isTRUE(enabled[[it$key]]), items)
      lapply(keep, function(it) it$inst)
    })
  })
}
