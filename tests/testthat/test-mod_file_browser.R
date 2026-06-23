test_that("navigator initializes to the single source root and lists its children", {
  src <- FakeSource$new(name = "a", root = "/p",
    data = list("/p/x.R" = 1, "/p/sub/b.R" = 1))
  shiny::testServer(
    mod_file_browser_server,
    args = list(active_sources = shiny::reactive(list(src))),
    {
      session$flushReact()
      expect_equal(current_dir(), "/p")
      e <- entries()
      expect_setequal(e$name, c("sub", "x.R"))
      expect_equal(e$type[e$name == "sub"], "dir")
    }
  )
})

test_that("clicking a directory descends; clicking a file selects it", {
  src <- FakeSource$new(name = "a", root = "/p",
    data = list("/p/x.R" = 1, "/p/sub/b.R" = 1))
  shiny::testServer(
    mod_file_browser_server,
    args = list(active_sources = shiny::reactive(list(src))),
    {
      session$flushReact()
      session$setInputs(clicked = "/p/sub")          # descend
      expect_equal(current_dir(), "/p/sub")
      expect_true("b.R" %in% entries()$name)
      session$setInputs(clicked = "/p/sub/b.R")      # select file
      expect_equal(session$returned(), "/p/sub/b.R")
    }
  )
})

test_that("breadcrumb click navigates back up", {
  src <- FakeSource$new(name = "a", root = "/p",
    data = list("/p/sub/b.R" = 1))
  shiny::testServer(
    mod_file_browser_server,
    args = list(active_sources = shiny::reactive(list(src))),
    {
      session$flushReact()
      session$setInputs(clicked = "/p/sub")
      expect_equal(current_dir(), "/p/sub")
      session$setInputs(crumb = "/p")
      expect_equal(current_dir(), "/p")
    }
  )
})

test_that("search returns matching files recursively", {
  src <- FakeSource$new(name = "a", root = "/p",
    data = list("/p/model.R" = 1, "/p/sub/model_helpers.R" = 1, "/p/sub/other.txt" = 1))
  shiny::testServer(
    mod_file_browser_server,
    args = list(active_sources = shiny::reactive(list(src))),
    {
      session$flushReact()
      session$setInputs(search = "model")
      res <- search_results()
      expect_setequal(basename(res$path), c("model.R", "model_helpers.R"))
    }
  )
})

test_that("no active sources -> nothing selected, current_dir NULL", {
  shiny::testServer(
    mod_file_browser_server,
    args = list(active_sources = shiny::reactive(list())),
    {
      session$flushReact()
      expect_null(current_dir())
      expect_null(session$returned())
    }
  )
})

test_that("multi-root view lists roots as dirs and descends on click", {
  a <- FakeSource$new(name = "a", root = "/a", data = list("/a/x.R" = 1))
  b <- FakeSource$new(name = "b", root = "/b", data = list("/b/y.R" = 1))
  shiny::testServer(
    mod_file_browser_server,
    args = list(active_sources = shiny::reactive(list(a, b))),
    {
      session$flushReact()
      expect_null(current_dir())                  # multi-root => NULL view
      expect_setequal(entries()$path, c("/a", "/b"))
      expect_true(all(entries()$type == "dir"))
      session$setInputs(clicked = "/a")
      expect_equal(current_dir(), "/a")
      session$setInputs(crumb = "")               # back to sources view
      expect_null(current_dir())
    }
  )
})

test_that("clicking a file in search results selects it", {
  src <- FakeSource$new(name = "a", root = "/p",
    data = list("/p/sub/model.R" = 1, "/p/other.txt" = 1))
  shiny::testServer(
    mod_file_browser_server,
    args = list(active_sources = shiny::reactive(list(src))),
    {
      session$flushReact()
      session$setInputs(search = "model")
      res <- search_results()
      expect_true(any(grepl("model.R$", res$path)))
      hit <- res$path[grepl("model.R$", res$path)][1]
      session$setInputs(clicked = hit)            # clicked observer uses search_results when searching
      expect_equal(session$returned(), hit)
    }
  )
})
