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
    shiny::div(class = "carousel-overview", shiny::uiOutput(ns("overview"))),
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
    view_range <- shiny::reactiveVal(NULL)   # c(from, to) POSIXct window; NULL = full span

    src_classes <- shiny::reactive({
      srcs <- active_sources()
      base <- if (length(srcs) == 0) {
        stats::setNames(character(0), character(0))
      } else {
        stats::setNames(
          vapply(srcs, function(s) paste0("src-", tolower(class(s)[[1]])), character(1)),
          vapply(srcs, function(s) s$name, character(1))
        )
      }
      c(base, stats::setNames("src-live", LIVE_SOURCE))   # one shared token for the live dot
    })

    full_range <- shiny::reactive({
      tl <- timeline()
      if (is.null(tl) || nrow(tl) == 0) return(NULL)
      c(min(tl$time), max(tl$time))
    })
    eff_range <- shiny::reactive(view_range() %||% full_range())

    visible_tl <- shiny::reactive({
      tl <- timeline()
      if (is.null(tl) || nrow(tl) == 0) return(tl)
      vr <- eff_range()
      if (is.null(vr)) return(tl)
      tl[tl$time >= vr[[1]] & tl$time <= vr[[2]], , drop = FALSE]
    })

    # New file -> drop any brush window.
    shiny::observeEvent(timeline(), { view_range(NULL) }, ignoreNULL = FALSE)

    # Visible frames changed (new file, brush applied, or brush cleared) -> reset to newest visible, clear pins.
    shiny::observeEvent(visible_tl(), {
      n <- nrow(visible_tl())
      if (is.null(n) || n == 0) { right_idx(NULL); left_idx(NULL) }
      else { right_idx(n); left_idx(if (n > 1) n - 1L else NULL) }
      left_pin(FALSE); right_pin(FALSE); next_click("left")
      shiny::updateActionButton(session, "pin_left",  label = "\U0001F4CC Pin left")
      shiny::updateActionButton(session, "pin_right", label = "\U0001F4CC Pin right")
    }, ignoreNULL = FALSE)

    n_frames <- shiny::reactive({ tl <- visible_tl(); if (is.null(tl)) 0L else nrow(tl) })
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
      left_pin(!left_pin()); next_click("left")
      shiny::updateActionButton(session, "pin_left",
        label = if (left_pin()) "\U0001F4CC Unpin left" else "\U0001F4CC Pin left")
    })
    shiny::observeEvent(input$pin_right, {
      right_pin(!right_pin()); next_click("left")
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

    # Brush window (date range). Wired now; the slider UI that drives it comes later.
    shiny::observeEvent(input$brush, {
      vr <- input$brush
      if (is.null(vr) || length(vr) != 2) return()
      from <- as.POSIXct(paste0(as.Date(vr[[1]]), " 00:00:00"), tz = "UTC")
      to   <- as.POSIXct(paste0(as.Date(vr[[2]]), " 23:59:59"), tz = "UTC")
      fr <- full_range()
      # A brush that covers the whole span means "no window" -> keep view_range NULL.
      if (!is.null(fr) && as.Date(from) <= as.Date(fr[[1]]) && as.Date(to) >= as.Date(fr[[2]])) {
        view_range(NULL)
      } else {
        view_range(c(from, to))
      }
    }, ignoreInit = TRUE)

    pane_content <- function(i) {
      tl <- visible_tl()
      if (is.null(i) || nrow(tl) == 0 || i < 1 || i > nrow(tl)) return(NULL)
      fetch_content(selected_path(), as.list(tl[i, ]), active_sources())
    }

    frame_label <- function(i) {
      tl <- visible_tl()
      if (is.null(i) || nrow(tl) == 0 || i < 1 || i > nrow(tl)) return(NULL)
      base <- tl$label[i]
      tok  <- if (tl$source[i] %in% names(src_classes())) src_classes()[[tl$source[i]]] else ""
      prefix <- if (identical(tok, "src-gitsource")) paste0(substr(tl$id[i], 1, 8), " ") else ""
      sprintf("%s%s (%s)", prefix, base, format(tl$time[i], "%Y-%m-%d %H:%M"))
    }

    output$overview <- shiny::renderUI({
      tl <- timeline(); fr <- full_range()
      if (is.null(tl) || nrow(tl) < 2 || is.null(fr)) return(NULL)
      d1 <- as.Date(fr[[1]]); d2 <- as.Date(fr[[2]])
      if (d1 >= d2) return(NULL)                 # single-day span: detail track already shows it (no day-granular brush)
      pos <- time_positions(tl$time, fr[[1]], fr[[2]])
      sc  <- src_classes()
      dots <- lapply(seq_len(nrow(tl)), function(i) {
        token <- if (tl$source[i] %in% names(sc)) sc[[tl$source[i]]] else "src-unknown"
        shiny::span(class = paste("ov-dot", token), style = sprintf("left:%.4f%%;", pos[i]))
      })
      shiny::tagList(
        shiny::div(class = "ov-strip", do.call(shiny::tagList, dots)),
        shiny::sliderInput(ns("brush"), NULL, min = d1, max = d2, value = c(d1, d2),
                           width = "100%", timeFormat = "%b %d, %Y")
      )
    })

    output$crumb <- shiny::renderText({
      tl <- visible_tl(); full <- timeline()
      if (is.null(right_idx()) || nrow(tl) == 0) return("No history")
      n <- nrow(tl); m <- if (is.null(full)) n else nrow(full)
      windowed <- n < m
      base <- sprintf("%s \u00B7 %d frame%s", basename(selected_path() %||% ""), n, if (n == 1) "" else "s")
      if (windowed) paste0(base, sprintf(" (of %d)", m)) else base
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
      tl <- visible_tl(); if (is.null(tl) || nrow(tl) == 0) return(NULL)
      er <- eff_range()
      render_timeline_bar(tl, left_idx(), right_idx(), left_pin(), right_pin(),
                          src_classes(), ns, labeller = frame_label,
                          from = er[[1]], to = er[[2]])
    })

    list(left_idx = left_idx, right_idx = right_idx, left_pin = left_pin,
         right_pin = right_pin, next_click = next_click,
         view_range = view_range, visible_tl = visible_tl)
  })
}

#' Render two content panes (text diff / image side-by-side / binary summary)
#' @noRd
render_compare <- function(left, right, path, view,
                           left_label = NULL, right_label = NULL,
                           left_pinned = FALSE, right_pinned = FALSE) {
  pin_lbl    <- function(lbl, pinned) if (isTRUE(pinned)) paste0("\U0001F4CC ", lbl %||% "") else (lbl %||% "")
  side_label <- function(lbl, content, pinned) {
    base <- pin_lbl(lbl, pinned)
    if (!is.null(content)) paste0(base, "  #", content$hash) else base
  }
  if (is.null(right)) return(shiny::div(class = "carousel-empty", "Nothing to show."))

  badge <- if (!is.null(left)) {
    if (identical(left$hash, right$hash)) {
      shiny::div(class = "hash-badge identical",
                 sprintf("\u2713 identical content  #%s", right$hash))
    } else {
      shiny::div(class = "hash-badge differs", "content differs")
    }
  } else NULL

  type <- right$type
  body <- if (identical(type, "text")) {
    left_lines <- if (is.null(left)) character() else left$lines
    html <- render_text_diff(left_lines, right$lines, mode = view,
                             a_label = side_label(left_label %||% "(no earlier version)", left, left_pinned),
                             b_label = side_label(right_label, right, right_pinned))
    shiny::HTML(html)
  } else if (identical(type, "image")) {
    img_pane <- function(content, lbl, pinned) shiny::div(class = "img-pane",
      shiny::div(class = "pane-label", side_label(lbl, content, pinned)),
      if (!is.null(content)) shiny::img(src = image_data_uri(content$bytes, path), width = "100%")
      else shiny::div(class = "carousel-empty", "(no earlier version)"))
    shiny::div(class = "img-compare",
      img_pane(left, left_label, left_pinned), img_pane(right, right_label, right_pinned))
  } else {
    bin_pane <- function(content, lbl, pinned) shiny::div(class = "binary-pane",
      shiny::div(class = "pane-label", side_label(lbl, content, pinned)),
      shiny::tags$p(if (!is.null(content)) binary_summary(content$bytes) else "(no earlier version)"))
    shiny::div(class = "binary-compare",
      bin_pane(left, left_label, left_pinned), bin_pane(right, right_label, right_pinned))
  }

  shiny::tagList(badge, body)
}

#' Render the time-proportional timeline (dots placed by timestamp + adaptive tick axis)
#' @noRd
render_timeline_bar <- function(tl, left_i, right_i, left_pin = FALSE, right_pin = FALSE,
                                src_classes = NULL, ns = identity, labeller = NULL,
                                from = NULL, to = NULL) {
  n <- nrow(tl)
  if (n == 0) return(NULL)
  from <- from %||% min(tl$time)
  to   <- to   %||% max(tl$time)
  pos <- time_positions(tl$time, from, to)

  dots <- lapply(seq_len(n), function(i) {
    token <- if (!is.null(src_classes) && tl$source[i] %in% names(src_classes)) src_classes[[tl$source[i]]] else "src-unknown"
    cls <- paste("tl-dot", token)
    if (!is.null(left_i)  && i == left_i)  cls <- paste(cls, "is-left")
    if (!is.null(right_i) && i == right_i) cls <- paste(cls, "is-right")
    if ((isTRUE(left_pin)  && !is.null(left_i)  && i == left_i) ||
        (isTRUE(right_pin) && !is.null(right_i) && i == right_i)) cls <- paste(cls, "is-pinned")
    title <- if (!is.null(labeller)) labeller(i) else paste(tl$source[i], format(tl$time[i]))
    shiny::tags$a(href = "#", class = cls, `data-idx` = i, title = title,
      style = sprintf("left:%.4f%%;", pos[i]),
      onclick = sprintf("Shiny.setInputValue('%s', this.getAttribute('data-idx'), {priority:'event'});", ns("frame")))
  })

  ticks <- time_axis_ticks(from, to)
  tpos  <- time_positions(ticks$time, from, to)
  tick_els <- lapply(seq_len(nrow(ticks)), function(j) {
    shiny::div(class = "tl-tick", style = sprintf("left:%.4f%%;", tpos[j]),
      shiny::div(class = "tl-tick-mark"),
      shiny::div(class = "tl-tick-label", ticks$label[j]))
  })

  shiny::div(class = "tl-track",
    do.call(shiny::tagList, tick_els),
    do.call(shiny::tagList, dots))
}
