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

test_that("GitSource list_tree returns files under the repo", {
  repo <- make_fixture_repo()
  src <- GitSource$new(repo = repo, name = "git")
  tr <- src$list_tree(file.path(repo, "R"))
  expect_true(any(basename(tr$path) == "model.R"))
})

test_that("GitSource list_tree(NULL) lists all tracked files recursively", {
  repo <- make_fixture_repo()
  src <- GitSource$new(repo = repo, name = "git")
  tr <- src$list_tree(NULL)          # how the file browser calls it (repo root)
  expect_true(any(basename(tr$path) == "model.R"))
  expect_true(all(tr$type == "file"))
  # the file lives under R/, so a recursive listing must include the nested path
  expect_true(any(grepl("R/model.R$", tr$path)))
})
