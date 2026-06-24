test_that("build_timeline merges sources sorted by time, tagging source name", {
  g <- fake_history("/p/a.R", list(
    list(id = "g1", label = "commit", time = 300, content = "v3")
  ), name = "git")
  z <- fake_history("/p/a.R", list(
    list(id = "z1", label = "snap", time = 100, content = "v1"),
    list(id = "z2", label = "snap", time = 200, content = "v2")
  ), name = "zfs")

  tl <- build_timeline("/p/a.R", list(g, z))
  expect_equal(tl$source, c("zfs", "zfs", "git"))
  expect_equal(tl$id, c("z1", "z2", "g1"))
  expect_true(all(diff(as.numeric(tl$time)) > 0))
})

test_that("build_timeline isolates a failing source and warns", {
  ok  <- fake_history("/p/a.R", list(list(id = "1", label = "x", time = 100, content = "a")), name = "ok")
  bad <- fake_history("/p/a.R", list(), name = "bad", fail = TRUE)

  expect_warning(tl <- build_timeline("/p/a.R", list(ok, bad)), class = "snapp_source_error")
  expect_equal(tl$source, "ok")
})

test_that("build_timeline returns empty tibble for no path/sources", {
  expect_equal(nrow(build_timeline(NULL, list())), 0)
})

test_that("build_timeline appends a single Current row for an on-disk file", {
  tmp <- withr::local_tempfile(fileext = ".R")
  writeBin(charToRaw("x <- 1"), tmp)
  known <- as.POSIXct("2030-01-01", tz = "UTC"); Sys.setFileTime(tmp, known)
  s <- fake_history(tmp, list(
    list(id = "g1", label = "commit", time = 100, content = "old")
  ), name = "git")

  tl <- build_timeline(tmp, list(s))

  expect_equal(nrow(tl), 2L)
  expect_equal(sum(tl$source == LIVE_SOURCE), 1L)
  live <- tl[tl$source == LIVE_SOURCE, ]
  expect_equal(live$id, LIVE_ID)
  expect_equal(live$label, "Current")
  expect_equal(as.numeric(live$time), as.numeric(known))
  expect_equal(tl$source[nrow(tl)], LIVE_SOURCE)   # mtime is newest -> sorts last
})

test_that("build_timeline omits the Current row when the path is not on disk", {
  s <- fake_history("/p/a.R", list(
    list(id = "1", label = "x", time = 100, content = "a")
  ), name = "git")

  tl <- build_timeline("/p/a.R", list(s))

  expect_false(LIVE_SOURCE %in% tl$source)
  expect_equal(nrow(tl), 1L)
})

test_that("build_timeline shows a lone Current row when the file has no source history", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeBin(charToRaw("hi"), tmp)
  s <- fake_history("/elsewhere/x.txt", list(), name = "git")  # no snapshots for tmp

  tl <- build_timeline(tmp, list(s))

  expect_equal(nrow(tl), 1L)
  expect_equal(tl$source, LIVE_SOURCE)
})
