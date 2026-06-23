#' File navigator UI (breadcrumb + one-level list + search)
#' @noRd
mod_file_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "nav-search",
      shiny::textInput(ns("search"), NULL, placeholder = "Search files\u2026"),
      shiny::checkboxInput(ns("recursive"), "Recursive", value = FALSE)
    ),
    shiny::uiOutput(ns("breadcrumb")),
    shiny::uiOutput(ns("listing"))
  )
}

#' File navigator server
#' @param id Module id.
#' @param active_sources reactive list of enabled SnapshotSource instances.
#' @return reactive selected absolute file path (or NULL).
#' @noRd
mod_file_browser_server <- function(id, active_sources) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_dir   <- shiny::reactiveVal(NULL)   # dir being browsed; NULL = multi-root "sources" view
    selected_file <- shiny::reactiveVal(NULL)

    roots <- shiny::reactive({
      srcs <- active_sources()
      if (length(srcs) == 0) return(character(0))
      unique(vapply(srcs, function(s) as.character(s$root()), character(1)))
    })

    default_dir <- function(rs) {
      wd <- tryCatch(as.character(fs::path_abs(getwd())), error = function(e) NULL)
      if (!is.null(wd) && any(vapply(rs, function(r) is_ancestor_or_equal(r, wd), logical(1)))) return(wd)
      if (length(rs) == 1) rs[[1]] else NULL
    }

    # Initialize / repair current_dir when the source set changes.
    shiny::observeEvent(active_sources(), {
      rs <- roots(); cd <- current_dir()
      if (length(rs) == 0) { current_dir(NULL); return() }
      under <- !is.null(cd) && any(vapply(rs, function(r) is_ancestor_or_equal(r, cd), logical(1)))
      if (!under) current_dir(default_dir(rs))
    }, ignoreNULL = FALSE)

    searching <- shiny::reactive(!is.null(input$search) && nzchar(input$search))

    entries <- shiny::reactive({
      srcs <- active_sources(); cd <- current_dir()
      empty <- tibble::tibble(name = character(), path = character(), type = character())
      if (length(srcs) == 0) return(empty)
      if (is.null(cd)) {                       # multi-root view: list roots as dirs
        rs <- roots()
        return(tibble::tibble(name = rs, path = rs, type = rep("dir", length(rs))))
      }
      merge_children(srcs, cd)
    })

    search_results <- shiny::reactive({
      empty <- tibble::tibble(name = character(), path = character(), type = character())
      if (!searching()) return(NULL)
      if (isTRUE(input$recursive)) {
        cd <- current_dir(); bases <- if (is.null(cd)) roots() else cd
        if (length(bases) == 0) return(empty)
        parts <- lapply(bases, function(b) find_files(active_sources(), b, input$search))
        res <- do.call(rbind, c(list(empty), parts))
        res <- res[!duplicated(res$path), , drop = FALSE]
        return(tibble::as_tibble(utils::head(res, 300L)))
      }
      # non-recursive: filter the current directory's immediate entries (dirs + files)
      q <- tolower(input$search)
      df <- entries()
      df[grepl(q, tolower(df$name), fixed = TRUE), , drop = FALSE]
    })

    output$breadcrumb <- shiny::renderUI({
      if (length(active_sources()) == 0 || (searching() && isTRUE(input$recursive))) return(NULL)
      cd <- current_dir(); rs <- roots()
      crumb_link <- function(path, label) {
        shiny::tags$a(href = "#", class = "nav-seg", `data-path` = path,
          onclick = sprintf("Shiny.setInputValue('%s', this.getAttribute('data-path'), {priority:'event'});", ns("crumb")),
          label)
      }
      items <- list()
      if (length(rs) > 1) items <- c(items, list(crumb_link("", "(sources)")))
      if (!is.null(cd)) {
        root <- rs[vapply(rs, function(r) is_ancestor_or_equal(r, cd), logical(1))]
        root <- if (length(root)) root[[1]] else cd
        segs <- character(0); cur <- as.character(cd)
        repeat {
          segs <- c(cur, segs)
          if (identical(cur, as.character(root))) break
          parent <- as.character(fs::path_dir(cur))
          if (identical(parent, cur)) break
          cur <- parent
        }
        for (p in segs) {
          lbl <- fs::path_file(p); if (!nzchar(lbl)) lbl <- p
          items <- c(items, list(crumb_link(p, lbl)))
        }
      }
      if (length(items) == 0) return(NULL)
      # interleave separators BETWEEN items only
      out <- list(items[[1]])
      for (i in seq_along(items)[-1]) out <- c(out, list(shiny::tags$span(class = "nav-sep", " / "), items[[i]]))
      shiny::div(class = "nav-crumb", do.call(shiny::tagList, out))
    })

    output$listing <- shiny::renderUI({
      if (length(active_sources()) == 0) {
        return(shiny::div(class = "nav-empty", "Enable a source in the Sources panel to browse files."))
      }
      df <- if (searching()) search_results() else entries()
      if (is.null(df) || nrow(df) == 0) {
        return(shiny::div(class = "nav-empty", if (searching()) "No matches." else "Empty directory."))
      }
      cap <- 500L
      total <- nrow(df)
      df_shown <- utils::head(df, cap)
      rows <- lapply(seq_len(nrow(df_shown)), function(i) {
        is_dir <- df_shown$type[i] == "dir"
        glyph  <- if (is_dir) "\U0001F4C2" else "\U0001F4C4"
        label  <- if (searching() && isTRUE(input$recursive)) df_shown$path[i] else df_shown$name[i]
        shiny::tags$a(href = "#",
          class = paste("nav-entry", if (is_dir) "is-dir" else "is-file"),
          `data-path` = df_shown$path[i],
          onclick = sprintf("Shiny.setInputValue('%s', this.getAttribute('data-path'), {priority:'event'});", ns("clicked")),
          shiny::span(class = "nav-icon", glyph), shiny::span(label))
      })
      ui <- shiny::div(class = "nav-list", do.call(shiny::tagList, rows))
      if (total > cap) {
        ui <- shiny::tagList(ui, shiny::div(class = "nav-empty",
          sprintf("\u2026and %d more \u2014 use search to narrow.", total - cap)))
      }
      ui
    })

    shiny::observeEvent(input$clicked, {
      p  <- input$clicked
      df <- if (searching()) search_results() else entries()
      row <- df[df$path == p, , drop = FALSE]
      typ <- if (nrow(row)) row$type[[1]] else "file"
      if (identical(typ, "dir")) {
        current_dir(p)
        shiny::updateTextInput(session, "search", value = "")
      } else {
        selected_file(p)
      }
    })

    shiny::observeEvent(input$crumb, {
      current_dir(if (identical(input$crumb, "")) NULL else input$crumb)
    })

    shiny::reactive(selected_file())
  })
}
