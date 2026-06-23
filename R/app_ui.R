#' The application User-Interface
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_sidebar(
      title = "snapp",
      sidebar = bslib::sidebar(
        width = 300,
        bslib::accordion(
          open = c("Sources", "Files"),
          bslib::accordion_panel("Sources", mod_sources_ui("sources")),
          bslib::accordion_panel("Files", mod_file_browser_ui("browser"))
        ),
        shiny::div(class = "sidebar-footer", mod_settings_ui("settings"))
      ),
      mod_carousel_ui("carousel")
    )
  )
}

#' Add external Resources to the Application
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(path = app_sys("app/www"), app_title = "snapp")
  )
}
