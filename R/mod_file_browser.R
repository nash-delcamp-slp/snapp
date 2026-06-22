#' File browser UI (tree + search)
#' @param id Module id.
#' @noRd
mod_file_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(ns("search"), NULL, placeholder = "Search files…"),
    shinyTree::shinyTreeOutput(ns("tree"))
  )
}

#' File browser server
#' @param id Module id.
#' @param active_sources reactive returning list of enabled sources.
#' @return reactive selected absolute file path (or NULL).
#' @noRd
mod_file_browser_server <- function(id, active_sources) {
  shiny::moduleServer(id, function(input, output, session) {
    files <- shiny::reactive({
      srcs <- active_sources()
      if (length(srcs) == 0) return(character())
      tr <- merge_tree(srcs)
      paths <- tr$path[tr$type == "file"]
      q <- input$search
      if (!is.null(q) && nzchar(q)) paths <- paths[grepl(q, paths, fixed = TRUE)]
      sort(paths)
    })

    output$tree <- shinyTree::renderTree({
      paths <- files()
      if (length(paths) == 0) return(list("(no files)" = ""))
      paths_to_tree(paths)
    })

    shiny::reactive({
      sel <- shinyTree::get_selected(input$tree, format = "names")
      if (length(sel) == 0) return(NULL)
      anc  <- attr(sel[[1]], "ancestry")   # ancestor node names from root, e.g. c("p","sub")
      leaf <- sel[[1]]                     # leaf node name, e.g. "b.R"
      paste0("/", paste(c(anc, leaf), collapse = "/"))
    })
  })
}

#' Convert a flat vector of absolute paths into a nested list for shinyTree,
#' where each leaf's value is its absolute path.
#' @noRd
paths_to_tree <- function(paths) {
  root <- list()
  for (p in paths) {
    parts <- strsplit(sub("^/", "", p), "/", fixed = TRUE)[[1]]
    root <- assign_nested(root, parts, p)
  }
  root
}

#' @noRd
assign_nested <- function(node, parts, fullpath) {
  if (length(parts) == 1) {
    node[[parts[[1]]]] <- structure("", sttype = "file")
    return(node)
  }
  child <- node[[parts[[1]]]]
  if (!is.list(child)) child <- list()
  node[[parts[[1]]]] <- assign_nested(child, parts[-1], fullpath)
  node
}
