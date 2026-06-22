# Build dataset_root with two timestamped snapshot dirs each containing R/model.R
make_snapdir_fixture <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  snap_base <- fs::dir_create(fs::path(root, ".zfs", "snapshot"))
  s1 <- fs::dir_create(fs::path(snap_base, "snap-2026-01-01T00:00:00", "R"))
  s2 <- fs::dir_create(fs::path(snap_base, "snap-2026-02-01T00:00:00", "R"))
  writeLines("v1", fs::path(s1, "model.R"))
  writeLines("v2", fs::path(s2, "model.R"))
  # the live file
  fs::dir_create(fs::path(root, "R"))
  writeLines("v2", fs::path(root, "R", "model.R"))
  root
}

test_that("SnapshotDirSource lists snapshots with time parsed from dir name", {
  root <- make_snapdir_fixture()
  src <- SnapshotDirSource$new(
    dataset_root = root,
    snapshot_glob = fs::path(root, ".zfs", "snapshot", "*"),
    time_from = list(regex = "snap-(.*)", format = "%Y-%m-%dT%H:%M:%S"),
    name = "zfs"
  )
  f <- fs::path(root, "R", "model.R")
  snaps <- src$list_snapshots(f)
  expect_equal(nrow(snaps), 2)
  expect_true(all(diff(as.numeric(snaps$time[order(snaps$time)])) > 0))
  oldest <- snaps$id[which.min(snaps$time)]
  expect_equal(trimws(rawToChar(src$read_file(f, oldest))), "v1")
})

test_that("SnapshotDirSource falls back to mtime when time_from is NULL", {
  root <- make_snapdir_fixture()
  src <- SnapshotDirSource$new(dataset_root = root,
    snapshot_glob = fs::path(root, ".zfs", "snapshot", "*"), name = "zfs")
  snaps <- src$list_snapshots(fs::path(root, "R", "model.R"))
  expect_equal(nrow(snaps), 2)
  expect_s3_class(snaps$time, "POSIXct")
})

test_that("find_zfs_root ascends to the dataset with a .zfs/snapshot dir", {
  root <- make_snapdir_fixture()
  expect_equal(normalizePath(find_zfs_root(fs::path(root, "R"))), normalizePath(root))
  expect_null(find_zfs_root(withr::local_tempdir()))
})
