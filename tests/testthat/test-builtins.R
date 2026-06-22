test_that("built-in git and zfs types are registered with detectors", {
  expect_true(all(c("git", "zfs") %in% source_types()))
})

test_that("git detector finds a repo by ascending", {
  repo <- make_fixture_repo()
  cands <- discover_sources(file.path(repo, "R", "model.R"))
  types <- vapply(cands, `[[`, character(1), "type")
  expect_true("git" %in% types)
  git_cand <- cands[[which(types == "git")[1]]]
  expect_equal(normalizePath(git_cand$params$repo), normalizePath(repo))
})

test_that("zfs preset fills a default snapshot_glob", {
  root <- make_snapdir_fixture()
  src <- new_source("zfs", list(dataset_root = root))
  expect_s3_class(src, "SnapshotDirSource")
  expect_match(src$snapshot_glob, "\\.zfs/snapshot/\\*$")
})
