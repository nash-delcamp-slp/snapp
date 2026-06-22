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
