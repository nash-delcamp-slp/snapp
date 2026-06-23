make_tl <- function(n = 3, source = "git") tibble::tibble(
  source = rep(source, n), id = paste0("c", seq_len(n)),
  label  = paste("commit", seq_len(n)),
  time   = as.POSIXct(seq_len(n) * 100, origin = "1970-01-01", tz = "UTC"))

carousel_args <- function(tl) list(
  timeline = shiny::reactive(tl),
  selected_path = shiny::reactive("/p/a.R"),
  active_sources = shiny::reactive(list()))

test_that("defaults to newest as right, previous as left, nothing pinned", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(3)), {
    session$flushReact()
    expect_equal(right_idx(), 3); expect_equal(left_idx(), 2)
    expect_false(left_pin()); expect_false(right_pin())
  })
})

test_that("stepping moves right with left following; clamps at ends", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(3)), {
    session$flushReact()
    session$setInputs(nxt = 1); expect_equal(right_idx(), 3)              # clamp top
    session$setInputs(prev = 1); expect_equal(right_idx(), 2); expect_equal(left_idx(), 1)
    session$setInputs(prev = 1); expect_equal(right_idx(), 1); expect_null(left_idx())  # no earlier version
  })
})

test_that("unpinned timeline clicks alternate left then right", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(5)), {
    session$flushReact()
    session$setInputs(frame = "2"); expect_equal(left_idx(), 2)
    session$setInputs(frame = "4"); expect_equal(right_idx(), 4)
    session$setInputs(frame = "1"); expect_equal(left_idx(), 1)
  })
})

test_that("pin left freezes left; stepping and clicks move right only", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(5)), {
    session$flushReact()
    session$setInputs(frame = "2")                 # left = 2 (first click)
    session$setInputs(pin_left = 1); expect_true(left_pin())
    session$setInputs(frame = "5"); expect_equal(right_idx(), 5); expect_equal(left_idx(), 2)
    session$setInputs(pin_left = 2); expect_false(left_pin())   # toggle unpin
  })
})

test_that("pin right freezes right; movement affects left", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(5)), {
    session$flushReact()
    session$setInputs(pin_right = 1); expect_true(right_pin())
    session$setInputs(frame = "1"); expect_equal(left_idx(), 1); expect_equal(right_idx(), 5)
    session$setInputs(nxt = 1); expect_equal(left_idx(), 2); expect_equal(right_idx(), 5)  # right frozen
  })
})

test_that("both pinned: stepping and clicks are no-ops", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(5)), {
    session$flushReact()
    session$setInputs(pin_left = 1, pin_right = 1)
    l <- left_idx(); r <- right_idx()
    session$setInputs(nxt = 1); session$setInputs(frame = "1")
    expect_equal(left_idx(), l); expect_equal(right_idx(), r)
  })
})

test_that("pin toggle resets click alternation to left", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(5)), {
    session$flushReact()
    session$setInputs(frame = "2")           # left = 2; next_click now "right"
    session$setInputs(pin_left = 1)          # pin (resets next_click)
    session$setInputs(pin_left = 2)          # unpin (resets next_click again)
    expect_equal(next_click(), "left")
    session$setInputs(frame = "4")           # should set LEFT, not right
    expect_equal(left_idx(), 4)
  })
})

test_that("single-frame timeline: stepping is a no-op (no self-diff)", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(1)), {
    session$flushReact()
    expect_equal(right_idx(), 1); expect_null(left_idx())
    session$setInputs(pin_right = 1)
    session$setInputs(prev = 1)              # would have set left=1 == right -> guarded
    expect_null(left_idx())
    expect_equal(right_idx(), 1)
  })
})

test_that("stale index beyond a newly-shorter timeline does not crash the body", {
  src <- FakeSource$new(name = "fake", root = "/p",
    data = list("/p/a.R" = list(
      list(id = "s1", label = "x", time = 1, content = "v1"),
      list(id = "s2", label = "y", time = 2, content = "v2"))))
  tl <- tibble::tibble(source = c("fake","fake"), id = c("s1","s2"),
                       label = c("x","y"),
                       time = as.POSIXct(c(1,2), origin = "1970-01-01", tz = "UTC"))
  shiny::testServer(
    mod_carousel_server,
    args = list(timeline = shiny::reactive(tl),
                selected_path = shiny::reactive("/p/a.R"),
                active_sources = shiny::reactive(list(src))),
    {
      session$flushReact()
      right_idx(99L)                      # stale index beyond nrow(tl) == 2
      expect_error(output$body, NA)       # rendering the body must NOT error
    }
  )
})
