test_that("carousel defaults to newest frame and clamps stepping", {
  tl <- tibble::tibble(
    source = c("zfs", "zfs", "git"), id = c("z1", "z2", "g1"),
    label = c("a", "b", "c"),
    time = as.POSIXct(c(100, 200, 300), origin = "1970-01-01", tz = "UTC")
  )
  shiny::testServer(
    mod_carousel_server,
    args = list(timeline = shiny::reactive(tl),
                selected_path = shiny::reactive("/p/a.R"),
                active_sources = shiny::reactive(list())),
    {
      session$flushReact()
      expect_equal(idx(), 3)             # newest
      expect_equal(left_idx(), 2)        # consecutive
      session$setInputs(nxt = 1)         # clamp at top
      expect_equal(idx(), 3)
      session$setInputs(prev = 1)
      expect_equal(idx(), 2)
    }
  )
})

test_that("pinning freezes the left pane to the pinned index", {
  tl <- tibble::tibble(
    source = c("git", "git", "git"), id = c("a", "b", "c"),
    label = c("a", "b", "c"),
    time = as.POSIXct(c(100, 200, 300), origin = "1970-01-01", tz = "UTC")
  )
  shiny::testServer(
    mod_carousel_server,
    args = list(timeline = shiny::reactive(tl),
                selected_path = shiny::reactive("/p/a.R"),
                active_sources = shiny::reactive(list())),
    {
      session$flushReact()
      idx(1); session$flushReact()
      session$setInputs(pin = 1)         # pin baseline at 1
      expect_equal(baseline(), 1)
      idx(3); session$flushReact()
      expect_equal(left_idx(), 1)        # left frozen at pin
      expect_equal(right_idx(), 3)
      session$setInputs(pin = 2)         # unpin
      expect_null(baseline())
    }
  )
})
