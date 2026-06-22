test_that("mod_sources instantiates configured sources and returns enabled ones", {
  # config_rv with one git source pointed at a fixture repo
  repo <- make_fixture_repo()
  cfg <- list(settings = list(default_view = "side-by-side", auto_discover = FALSE),
              sources = list(list(name = "g", type = "git", enabled = TRUE,
                                  params = list(repo = repo))))
  config_rv <- shiny::reactiveVal(cfg)
  shiny::testServer(
    mod_sources_server,
    args = list(config_rv = config_rv),
    {
      session$flushReact()
      # the enable checkbox id is en_<make.names("g")> = en_g; set it TRUE
      session$setInputs(en_g = TRUE)
      enabled <- session$returned()       # the returned reactive (list of instances)
      expect_length(enabled, 1)
      expect_equal(enabled[[1]]$name, "g")
      # disabling drops it
      session$setInputs(en_g = FALSE)
      expect_length(session$returned(), 0)
    }
  )
})
