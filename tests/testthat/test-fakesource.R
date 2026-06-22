test_that("FakeSource returns ordered-by-construction snapshots and content", {
  s <- fake_history("/proj/a.R", list(
    list(id = "s1", label = "first",  time = 100, content = c("x <- 1")),
    list(id = "s2", label = "second", time = 200, content = c("x <- 2"))
  ))
  snaps <- s$list_snapshots("/proj/a.R")
  expect_equal(nrow(snaps), 2)
  expect_equal(snaps$id, c("s1", "s2"))
  expect_equal(rawToChar(s$read_file("/proj/a.R", "s2")), "x <- 2")
})

test_that("FakeSource with fail=TRUE errors on list_snapshots", {
  expect_error(fake_history("/p", list(), fail = TRUE)$list_snapshots("/p"), "fake failure")
})
