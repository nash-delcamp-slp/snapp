test_that("fetch_content decodes text and routes by type", {
  s <- fake_history("/p/a.R", list(
    list(id = "1", label = "x", time = 1, content = c("line1", "line2"))
  ), name = "git")
  entry <- list(source = "git", id = "1")
  c1 <- fetch_content("/p/a.R", entry, list(s))
  expect_equal(c1$type, "text")
  expect_equal(c1$lines, c("line1", "line2"))
})

test_that("fetch_content marks binary content with NUL bytes", {
  s <- fake_history("/p/blob.bin", list(
    list(id = "1", label = "x", time = 1, content = as.raw(c(0x00, 0x01)))
  ), name = "zfs")
  c1 <- fetch_content("/p/blob.bin", list(source = "zfs", id = "1"), list(s))
  expect_equal(c1$type, "binary")
  expect_null(c1$lines)
})

test_that("fetch_content errors when the source name is not among sources", {
  s <- fake_history("/p/a.R", list(list(id = "1", label = "x", time = 1, content = "a")))
  expect_error(fetch_content("/p/a.R", list(source = "missing", id = "1"), list(s)),
               "No active source")
})
