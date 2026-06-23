test_that("configured + discovered sources are enabled by default", {
  repo <- make_fixture_repo()
  config_rv <- shiny::reactiveVal(list(
    settings = list(default_view = "side-by-side", auto_discover = TRUE),
    sources  = list(list(name = "g", type = "git", enabled = TRUE, params = list(repo = repo)))))
  disc <- shiny::reactiveVal(list(
    list(name = "zfs", type = "zfs", params = list(dataset_root = withr::local_tempdir()))))
  shiny::testServer(
    mod_sources_server,
    args = list(config_rv = config_rv, discovered = disc),
    {
      session$flushReact()
      nm <- vapply(session$returned(), function(s) s$name, character(1))
      expect_setequal(nm, c("g", "zfs"))           # both on by default (configured + discovered)
    }
  )
})

test_that("a toggled-off source stays off across a discovered() change (file select)", {
  repo <- make_fixture_repo()
  config_rv <- shiny::reactiveVal(list(
    settings = list(default_view = "side-by-side", auto_discover = TRUE), sources = list()))
  mk_disc <- function() list(
    list(name = "git", type = "git", params = list(repo = repo)),
    list(name = "zfs", type = "zfs", params = list(dataset_root = withr::local_tempdir())))
  disc <- shiny::reactiveVal(mk_disc())
  shiny::testServer(
    mod_sources_server,
    args = list(config_rv = config_rv, discovered = disc),
    {
      # NOTE: testServer can't simulate the browser DOM re-initialization that
      # originally reset the checkbox; this verifies the persistent-store plumbing
      # (enabled-sources reads the store, not the raw input). The real DOM-reset
      # fix is validated by manual browser testing.
      session$flushReact()
      expect_equal(length(session$returned()), 2)          # both on by default
      session$setInputs(en_zfs = FALSE)                    # user disables zfs
      session$flushReact()
      expect_setequal(vapply(session$returned(), function(s) s$name, character(1)), "git")
      disc(mk_disc())                                      # simulate selecting a new file -> discovered recomputes
      session$flushReact()
      expect_setequal(vapply(session$returned(), function(s) s$name, character(1)), "git")  # zfs STILL off
      session$setInputs(en_zfs = TRUE)                     # re-enable persists too
      session$flushReact()
      expect_setequal(vapply(session$returned(), function(s) s$name, character(1)), c("git", "zfs"))
    }
  )
})
