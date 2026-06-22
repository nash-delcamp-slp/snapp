test_that("merge_tree unions entries across sources and dedupes", {
  a <- FakeSource$new(name = "a", data = list("/p/x.R" = 1, "/p/y.R" = 1))
  b <- FakeSource$new(name = "b", data = list("/p/y.R" = 1, "/p/z.R" = 1))
  tr <- merge_tree(list(a, b))
  expect_setequal(tr$path, c("/p/x.R", "/p/y.R", "/p/z.R"))
  expect_true(all(tr$type == "file"))
})

test_that("merge_tree isolates a failing source", {
  good <- FakeSource$new(name = "g", data = list("/p/x.R" = 1))
  bad  <- FakeSource$new(name = "b", fail = TRUE)
  unlockBinding("list_tree", bad)
  bad$list_tree <- function(path = NULL, id = NULL) stop("boom")
  expect_warning(tr <- merge_tree(list(good, bad)), class = "snapp_source_error")
  expect_equal(tr$path, "/p/x.R")
})

test_that("merge_tree lists files from a real GitSource via list_tree(NULL)", {
  repo <- make_fixture_repo()
  src <- GitSource$new(repo = repo, name = "git")
  tr <- merge_tree(list(src))     # path = NULL, exactly as the file browser calls it
  expect_gt(nrow(tr), 0)
  expect_true(any(basename(tr$path) == "model.R"))
})
