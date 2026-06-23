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
  root <- withr::local_tempdir()
  snap_base <- fs::dir_create(fs::path(root, ".zfs", "snapshot"))
  s1 <- fs::dir_create(fs::path(snap_base, "snap-2026-01-01T00:00:00", "R"))
  s2 <- fs::dir_create(fs::path(snap_base, "snap-2026-02-01T00:00:00", "R"))
  f1 <- fs::path(s1, "model.R"); writeLines("v1", f1)
  f2 <- fs::path(s2, "model.R"); writeLines("v2", f2)
  # Explicitly set distinct file mtimes so the two versions never collapse
  Sys.setFileTime(f1, as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))
  Sys.setFileTime(f2, as.POSIXct("2026-02-01 00:00:00", tz = "UTC"))
  fs::dir_create(fs::path(root, "R"))
  writeLines("v2", fs::path(root, "R", "model.R"))
  src <- SnapshotDirSource$new(dataset_root = root,
    snapshot_glob = fs::path(root, ".zfs", "snapshot", "*"), name = "zfs")
  snaps <- src$list_snapshots(fs::path(root, "R", "model.R"))
  expect_equal(nrow(snaps), 2)
  expect_s3_class(snaps$time, "POSIXct")
  expect_true(all(diff(as.numeric(snaps$time)) > 0))   # ordered ascending
})

test_that("zfs preset times by file mtime and collapses identical versions", {
  root <- withr::local_tempdir()
  base <- fs::dir_create(fs::path(root, ".zfs", "snapshot"))
  mk <- function(snap, content, mtime) {
    d <- fs::dir_create(fs::path(base, snap, "sub", "deep"))
    f <- fs::path(d, "f.R"); writeLines(content, f)
    Sys.setFileTime(f, mtime); invisible(f)
  }
  t1 <- as.POSIXct("2026-01-01 09:00:00", tz = "UTC")
  t2 <- as.POSIXct("2026-03-01 09:00:00", tz = "UTC")
  mk("snap_incr-2026-01-01_0900", "v1", t1)   # version 1
  mk("snap_daily-2026-03-01_0900", "v2", t2)  # version 2
  mk("snap_daily-2026-03-02_0900", "v2", t2)  # unchanged -> same mtime -> collapses
  fs::dir_create(fs::path(root, "sub", "deep")); writeLines("v2", fs::path(root, "sub", "deep", "f.R"))

  src <- new_source("zfs", list(dataset_root = root))   # no time_from -> file mtime
  snaps <- src$list_snapshots(fs::path(root, "sub", "deep", "f.R"))
  expect_equal(nrow(snaps), 2)                          # v2 snapshots collapsed
  expect_true(all(diff(as.numeric(snaps$time)) > 0))    # ordered by file mtime
  expect_equal(snaps$label[1], "snap_incr-2026-01-01_0900")  # label is the snapshot dir name
  expect_equal(snaps$label[2], "snap_daily-2026-03-01_0900")   # earliest snapshot of the v2 version is kept
  # earliest version reads "v1"; id is the snapshot dir
  expect_equal(trimws(rawToChar(src$read_file(fs::path(root,"sub","deep","f.R"), snaps$id[1]))), "v1")
})

test_that("find_zfs_root ascends to the dataset with a .zfs/snapshot dir", {
  root <- make_snapdir_fixture()
  expect_equal(normalizePath(find_zfs_root(fs::path(root, "R"))), normalizePath(root))
  expect_null(find_zfs_root(withr::local_tempdir()))
})

test_that("SnapshotDirSource root + list_children list the LIVE dataset, one level", {
  root <- make_snapdir_fixture()
  src <- SnapshotDirSource$new(dataset_root = root,
    snapshot_glob = fs::path(root, ".zfs", "snapshot", "*"), name = "zfs")
  expect_equal(normalizePath(src$root()), normalizePath(root))
  top <- src$list_children(NULL)
  expect_true("R" %in% top$name)                 # live dir under dataset_root
  expect_false(".zfs" %in% top$name)             # hidden .zfs excluded
  kids <- src$list_children(fs::path(root, "R"))
  expect_true("model.R" %in% kids$name)
  expect_true(any(grepl("R/model.R$", kids$path)))
  expect_false(any(grepl("\\.zfs/snapshot", kids$path)))   # live path, not snapshot
})

test_that("SnapshotDirSource list_children is fault-tolerant on unreadable dirs", {
  testthat::skip_if(unname(Sys.info()["effective_user"]) == "root", "chmod ineffective as root")
  root <- withr::local_tempdir()
  fs::dir_create(fs::path(root, ".zfs", "snapshot"))
  locked <- fs::dir_create(fs::path(root, "locked"))
  fs::file_create(fs::path(locked, "secret.txt"))
  Sys.chmod(locked, mode = "000")
  on.exit(Sys.chmod(locked, mode = "755"), add = TRUE)   # so tempdir cleanup works
  src <- SnapshotDirSource$new(dataset_root = root,
    snapshot_glob = fs::path(root, ".zfs", "snapshot", "*"), name = "zfs")
  # listing the locked dir must NOT error -> empty tibble
  kids <- src$list_children(locked)
  expect_s3_class(kids, "tbl_df")
  expect_equal(nrow(kids), 0)
})
