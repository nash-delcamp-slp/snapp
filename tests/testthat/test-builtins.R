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

test_that("zfs preset parses snapshot timestamps from dir names, ordering correctly", {
  root <- withr::local_tempdir()
  base <- fs::dir_create(fs::path(root, ".zfs", "snapshot"))
  for (nm in c("snap_daily-2026-05-31_1815", "snap_incr-2026-05-31_0915",
               "snap_incr-2026-05-31_1315", "snap_monthly-2026-06-01_0100")) {
    d <- fs::dir_create(fs::path(base, nm, "sub"))
    writeLines("x", fs::path(d, "f.R"))
  }
  fs::dir_create(fs::path(root, "sub")); writeLines("x", fs::path(root, "sub", "f.R"))

  src <- new_source("zfs", list(dataset_root = root))
  snaps <- src$list_snapshots(fs::path(root, "sub", "f.R"))
  expect_equal(nrow(snaps), 4)
  expect_equal(length(unique(snaps$time)), 4)
  expect_equal(snaps$label[order(snaps$time)], c(
    "snap_incr-2026-05-31_0915", "snap_incr-2026-05-31_1315",
    "snap_daily-2026-05-31_1815", "snap_monthly-2026-06-01_0100"))
})
