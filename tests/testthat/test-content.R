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

test_that("fetch_content attaches a content hash", {
  s <- fake_history("/p/a.R", list(list(id = "1", label = "x", time = 1, content = c("l1","l2"))), name = "git")
  c1 <- fetch_content("/p/a.R", list(source = "git", id = "1"), list(s))
  expect_equal(c1$hash, content_hash(c1$bytes))
  expect_equal(nchar(c1$hash), 12L)
})

test_that("fetch_content reads the live on-disk file for the Current entry", {
  tmp <- withr::local_tempfile(fileext = ".R")
  writeBin(charToRaw("line1\nline2"), tmp)

  c1 <- fetch_content(tmp, list(source = LIVE_SOURCE, id = LIVE_ID), list())

  expect_equal(c1$type, "text")
  expect_equal(c1$lines, c("line1", "line2"))
  expect_equal(c1$hash, content_hash(c1$bytes))
  expect_equal(nchar(c1$hash), 12L)
})

test_that("fetch_content classifies a live binary file (NUL bytes) and emits no lines", {
  tmp <- withr::local_tempfile(fileext = ".bin")
  writeBin(as.raw(c(0x00, 0x01, 0x02)), tmp)

  c1 <- fetch_content(tmp, list(source = LIVE_SOURCE, id = LIVE_ID), list())

  expect_equal(c1$type, "binary")
  expect_null(c1$lines)
})

test_that("live and snapshot bytes that match produce equal hashes", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeBin(charToRaw("same"), tmp)
  s <- fake_history(tmp, list(
    list(id = "1", label = "x", time = 1, content = "same")
  ), name = "git")

  live <- fetch_content(tmp, list(source = LIVE_SOURCE, id = LIVE_ID), list(s))
  snap <- fetch_content(tmp, list(source = "git", id = "1"), list(s))

  expect_equal(live$hash, snap$hash)
})
