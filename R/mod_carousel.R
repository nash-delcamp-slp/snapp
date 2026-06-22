#' Diff carousel UI
#' @noRd
mod_carousel_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::div(class = "carousel-bar",
        shiny::actionButton(ns("prev"), "◀", class = "btn-sm"),
        shiny::span(class = "carousel-crumb", shiny::textOutput(ns("crumb"), inline = TRUE)),
        shiny::actionButton(ns("nxt"), "▶", class = "btn-sm"),
        shiny::actionButton(ns("pin"), "\U0001F4CC Pin", class = "btn-sm"),
        shiny::radioButtons(ns("view"), NULL, inline = TRUE,
                            choices = c("side-by-side", "unified"), selected = "side-by-side")
      )
    ),
    shiny::uiOutput(ns("body")),
    shiny::div(class = "carousel-timeline", shiny::uiOutput(ns("timeline")))
  )
}

#' Diff carousel server
#' @param id Module id.
#' @param timeline reactive tibble(source,id,label,time).
#' @param selected_path reactive absolute file path.
#' @param active_sources reactive list of sources.
#' @param default_view reactive/character starting view mode.
#' @noRd
mod_carousel_server <- function(id, timeline, selected_path, active_sources,
                                default_view = shiny::reactive("side-by-side")) {
  shiny::moduleServer(id, function(input, output, session) {
    idx      <- shiny::reactiveVal(NULL)   # current frame index (1-based)
    baseline <- shiny::reactiveVal(NULL)   # pinned index or NULL

    # When the timeline changes, default to the newest frame, clear pin.
    shiny::observeEvent(timeline(), {
      n <- nrow(timeline())
      idx(if (n > 0) n else NULL)
      baseline(NULL)
    }, ignoreNULL = FALSE)

    n_frames <- shiny::reactive(nrow(timeline()))

    step <- function(delta) {
      n <- n_frames(); cur <- idx()
      if (is.null(cur) || n == 0) return()
      idx(max(1, min(n, cur + delta)))
    }
    shiny::observeEvent(input$prev, step(-1))
    shiny::observeEvent(input$nxt,  step(+1))
    shiny::observeEvent(input$pin, {
      baseline(if (is.null(baseline())) idx() else NULL)
    })

    # Indices for the two panes.
    left_idx <- shiny::reactive({
      if (!is.null(baseline())) return(baseline())
      cur <- idx(); if (is.null(cur)) return(NULL)
      if (cur > 1) cur - 1 else NULL
    })
    right_idx <- shiny::reactive(idx())

    pane_content <- function(i) {
      if (is.null(i)) return(NULL)
      tl <- timeline()
      fetch_content(selected_path(), as.list(tl[i, ]), active_sources())
    }

    output$crumb <- shiny::renderText({
      tl <- timeline(); cur <- idx()
      if (is.null(cur) || nrow(tl) == 0) return("No history")
      sprintf("%s · frame %d of %d · %s",
              basename(selected_path() %||% ""), cur, nrow(tl), tl$source[cur])
    })

    output$body <- shiny::renderUI({
      if (is.null(idx()) || n_frames() == 0) {
        return(shiny::div(class = "carousel-empty", "Select a file with history to compare."))
      }
      view <- input$view %||% default_view()
      lc <- pane_content(left_idx()); rc <- pane_content(right_idx())
      render_compare(lc, rc, selected_path(), view)
    })

    output$timeline <- shiny::renderUI({
      tl <- timeline(); if (nrow(tl) == 0) return(NULL)
      render_timeline_bar(tl, idx(), baseline())
    })

    # expose state for tests
    list(idx = idx, baseline = baseline, left_idx = left_idx, right_idx = right_idx)
  })
}

#' Render two content panes (text diff / image side-by-side / binary summary)
#' @noRd
render_compare <- function(left, right, path, view) {
  if (is.null(right)) return(shiny::div("Nothing to show."))
  type <- right$type
  if (identical(type, "text")) {
    html <- render_text_diff(left$lines, right$lines, mode = view)
    return(shiny::HTML(html))
  }
  if (identical(type, "image")) {
    left_img <- if (!is.null(left)) shiny::img(src = image_data_uri(left$bytes, path), width = "100%")
    right_img <- shiny::img(src = image_data_uri(right$bytes, path), width = "100%")
    return(shiny::div(class = "img-compare",
                      shiny::div(class = "img-pane", left_img),
                      shiny::div(class = "img-pane", right_img)))
  }
  shiny::div(class = "binary-compare",
             shiny::tags$p(if (!is.null(left)) binary_summary(left$bytes)),
             shiny::tags$p(binary_summary(right$bytes)))
}

#' Render the timeline scrubber bar (source-colored dots)
#' @noRd
render_timeline_bar <- function(tl, cur, base) {
  n <- nrow(tl)
  dots <- lapply(seq_len(n), function(i) {
    cls <- paste("tl-dot", paste0("src-", make.names(tl$source[i])))
    if (!is.null(cur) && i == cur) cls <- paste(cls, "is-current")
    if (!is.null(base) && i == base) cls <- paste(cls, "is-baseline")
    shiny::span(class = cls, title = paste(tl$source[i], format(tl$time[i])))
  })
  shiny::div(class = "tl-track", dots)
}
