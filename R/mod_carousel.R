#' Diff carousel UI
#' @noRd
mod_carousel_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::div(class = "carousel-bar",
        shiny::actionButton(ns("pin_left"), "\U0001F4CC Pin left", class = "btn-sm"),
        shiny::actionButton(ns("prev"), "\u25C0", class = "btn-sm"),
        shiny::span(class = "carousel-crumb", shiny::textOutput(ns("crumb"), inline = TRUE)),
        shiny::actionButton(ns("nxt"), "\u25B6", class = "btn-sm"),
        shiny::actionButton(ns("pin_right"), "\U0001F4CC Pin right", class = "btn-sm"),
        shiny::radioButtons(ns("view"), NULL, inline = TRUE,
                            choices = c("side-by-side", "unified"), selected = "side-by-side")
      )
    ),
    shiny::div(class = "carousel-timeline", shiny::uiOutput(ns("timeline"))),
    shiny::uiOutput(ns("body"))
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
    ns <- session$ns
    left_idx   <- shiny::reactiveVal(NULL)
    right_idx  <- shiny::reactiveVal(NULL)
    left_pin   <- shiny::reactiveVal(FALSE)
    right_pin  <- shiny::reactiveVal(FALSE)
    next_click <- shiny::reactiveVal("left")

    src_classes <- shiny::reactive({
      srcs <- active_sources()
      if (length(srcs) == 0) return(stats::setNames(character(0), character(0)))
      stats::setNames(
        vapply(srcs, function(s) paste0("src-", tolower(class(s)[[1]])), character(1)),
        vapply(srcs, function(s) s$name, character(1))
      )
    })

    shiny::observeEvent(timeline(), {
      n <- nrow(timeline())
      if (is.null(n) || n == 0) { right_idx(NULL); left_idx(NULL) }
      else { right_idx(n); left_idx(if (n > 1) n - 1 else NULL) }
      left_pin(FALSE); right_pin(FALSE); next_click("left")
      shiny::updateActionButton(session, "pin_left",  label = "\U0001F4CC Pin left")
      shiny::updateActionButton(session, "pin_right", label = "\U0001F4CC Pin right")
    }, ignoreNULL = FALSE)

    n_frames <- shiny::reactive(nrow(timeline()))
    clamp <- function(x, n) max(1L, min(n, x))

    step <- function(delta) {
      n <- n_frames(); if (is.null(n) || n <= 1) return()
      lp <- left_pin(); rp <- right_pin()
      if (!lp && !rp) {
        r <- clamp((right_idx() %||% n) + delta, n); right_idx(r)
        left_idx(if (r > 1) r - 1L else NULL)
      } else if (lp && !rp) {
        right_idx(clamp((right_idx() %||% n) + delta, n))
      } else if (rp && !lp) {
        left_idx(clamp((left_idx() %||% 1L) + delta, n))
      }
    }
    shiny::observeEvent(input$prev, step(-1L))
    shiny::observeEvent(input$nxt,  step(+1L))
    shiny::observeEvent(input$pin_left, {
      left_pin(!left_pin())
      next_click("left")
      shiny::updateActionButton(session, "pin_left",
        label = if (left_pin()) "\U0001F4CC Unpin left" else "\U0001F4CC Pin left")
    })
    shiny::observeEvent(input$pin_right, {
      right_pin(!right_pin())
      next_click("left")
      shiny::updateActionButton(session, "pin_right",
        label = if (right_pin()) "\U0001F4CC Unpin right" else "\U0001F4CC Pin right")
    })

    shiny::observeEvent(input$frame, {
      i <- suppressWarnings(as.integer(input$frame)); n <- n_frames()
      if (is.null(n) || n == 0 || is.na(i) || i < 1 || i > n) return()
      lp <- left_pin(); rp <- right_pin()
      if (!lp && !rp) {
        if (identical(next_click(), "left")) { left_idx(i); next_click("right") }
        else { right_idx(i); next_click("left") }
      } else if (lp && !rp) { right_idx(i) }
      else if (rp && !lp) { left_idx(i) }
    })

    pane_content <- function(i) {
      tl <- timeline()
      if (is.null(i) || nrow(tl) == 0 || i < 1 || i > nrow(tl)) return(NULL)
      fetch_content(selected_path(), as.list(tl[i, ]), active_sources())
    }

    frame_label <- function(i) {
      tl <- timeline()
      if (is.null(i) || nrow(tl) == 0 || i < 1 || i > nrow(tl)) return(NULL)
      base <- tl$label[i]
      tok  <- if (tl$source[i] %in% names(src_classes())) src_classes()[[tl$source[i]]] else ""
      prefix <- if (identical(tok, "src-gitsource")) paste0(substr(tl$id[i], 1, 8), " ") else ""
      sprintf("%s%s (%s)", prefix, base, format(tl$time[i], "%Y-%m-%d %H:%M"))
    }

    output$crumb <- shiny::renderText({
      tl <- timeline()
      if (is.null(right_idx()) || nrow(tl) == 0) return("No history")
      sprintf("%s \u00B7 %d frame%s", basename(selected_path() %||% ""),
              nrow(tl), if (nrow(tl) == 1) "" else "s")
    })

    output$body <- shiny::renderUI({
      if (is.null(right_idx()) || n_frames() == 0) {
        return(shiny::div(class = "carousel-empty", "Select a file with history to compare."))
      }
      view <- input$view %||% default_view()
      render_compare(
        pane_content(left_idx()), pane_content(right_idx()), selected_path(), view,
        left_label = frame_label(left_idx()), right_label = frame_label(right_idx()),
        left_pinned = left_pin(), right_pinned = right_pin()
      )
    })

    output$timeline <- shiny::renderUI({
      tl <- timeline(); if (nrow(tl) == 0) return(NULL)
      render_timeline_bar(tl, left_idx(), right_idx(), left_pin(), right_pin(),
                          src_classes(), ns, labeller = frame_label)
    })

    list(left_idx = left_idx, right_idx = right_idx,
         left_pin = left_pin, right_pin = right_pin, next_click = next_click)
  })
}

#' Render two content panes (text diff / image side-by-side / binary summary)
#' @noRd
render_compare <- function(left, right, path, view,
                           left_label = NULL, right_label = NULL,
                           left_pinned = FALSE, right_pinned = FALSE) {
  pin_lbl <- function(lbl, pinned) if (isTRUE(pinned)) paste0("\U0001F4CC ", lbl %||% "") else (lbl %||% "")
  if (is.null(right)) return(shiny::div(class = "carousel-empty", "Nothing to show."))
  type <- right$type
  if (identical(type, "text")) {
    left_lines <- if (is.null(left)) character() else left$lines   # empty-left = "no earlier version"
    html <- render_text_diff(left_lines, right$lines, mode = view,
                             a_label = pin_lbl(left_label %||% "(no earlier version)", left_pinned),
                             b_label = pin_lbl(right_label, right_pinned))
    return(shiny::HTML(html))
  }
  if (identical(type, "image")) {
    img_pane <- function(content, lbl, pinned) shiny::div(class = "img-pane",
      shiny::div(class = "pane-label", pin_lbl(lbl, pinned)),
      if (!is.null(content)) shiny::img(src = image_data_uri(content$bytes, path), width = "100%")
      else shiny::div(class = "carousel-empty", "(no earlier version)"))
    return(shiny::div(class = "img-compare",
      img_pane(left, left_label, left_pinned), img_pane(right, right_label, right_pinned)))
  }
  bin_pane <- function(content, lbl, pinned) shiny::div(class = "binary-pane",
    shiny::div(class = "pane-label", pin_lbl(lbl, pinned)),
    shiny::tags$p(if (!is.null(content)) binary_summary(content$bytes) else "(no earlier version)"))
  shiny::div(class = "binary-compare",
    bin_pane(left, left_label, left_pinned), bin_pane(right, right_label, right_pinned))
}

#' Render the clickable timeline scrubber (left/right markers, pinned, source color)
#' @noRd
render_timeline_bar <- function(tl, left_i, right_i, left_pin = FALSE, right_pin = FALSE,
                                src_classes = NULL, ns = identity, labeller = NULL) {
  n <- nrow(tl)
  dots <- lapply(seq_len(n), function(i) {
    token <- if (!is.null(src_classes) && tl$source[i] %in% names(src_classes)) src_classes[[tl$source[i]]] else "src-unknown"
    cls <- paste("tl-dot", token)
    if (!is.null(left_i)  && i == left_i)  cls <- paste(cls, "is-left")
    if (!is.null(right_i) && i == right_i) cls <- paste(cls, "is-right")
    if ((isTRUE(left_pin)  && !is.null(left_i)  && i == left_i) ||
        (isTRUE(right_pin) && !is.null(right_i) && i == right_i)) cls <- paste(cls, "is-pinned")
    title <- if (!is.null(labeller)) labeller(i) else paste(tl$source[i], format(tl$time[i]))
    shiny::tags$a(href = "#", class = cls, `data-idx` = i, title = title,
      onclick = sprintf("Shiny.setInputValue('%s', this.getAttribute('data-idx'), {priority:'event'});", ns("frame")))
  })
  shiny::div(class = "tl-track", dots)
}
