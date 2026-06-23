test_that("find_git_root ascends to the repo", {
  repo <- make_fixture_repo()
  expect_equal(normalizePath(find_git_root(file.path(repo, "R"))), normalizePath(repo))
  expect_null(find_git_root(withr::local_tempdir()))
})

test_that("GitSource lists snapshots newest-info and reads blobs", {
  repo <- make_fixture_repo()
  src <- GitSource$new(repo = repo, name = "git")
  f <- file.path(repo, "R", "model.R")
  snaps <- src$list_snapshots(f)
  expect_equal(nrow(snaps), 2)
  expect_true(all(c("id", "label", "time") %in% names(snaps)))
  oldest <- snaps$id[which.min(snaps$time)]
  txt <- rawToChar(src$read_file(f, oldest))
  expect_match(txt, "tol <- 1e-4")
  expect_false(grepl("iter", txt))
})

test_that("GitSource root + list_children list one level with types", {
  repo <- make_fixture_repo()
  src <- GitSource$new(repo = repo, name = "git")
  expect_equal(normalizePath(src$root()), normalizePath(repo))
  top <- src$list_children(NULL)           # repo root
  expect_true("R" %in% top$name)
  expect_equal(top$type[top$name == "R"], "dir")
  kids <- src$list_children(file.path(repo, "R"))
  expect_true("model.R" %in% kids$name)
  expect_equal(kids$type[kids$name == "model.R"], "file")
  expect_true(any(grepl("R/model.R$", kids$path)))   # absolute live path
})
