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

test_that("zfs preset lists ALL snapshots containing a file, ordered by dir-name datetime", {
  root <- withr::local_tempdir()
  base <- fs::dir_create(fs::path(root, ".zfs", "snapshot"))
  snap_names <- c("snap_incr-2026-05-31_0915", "snap_incr-2026-05-31_1315",
                  "snap_daily-2026-05-31_1815", "snap_monthly-2026-06-01_0100")
  fixed_mtime <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  for (i in seq_along(snap_names)) {
    d <- fs::dir_create(fs::path(base, snap_names[i], "sub", "deep"))
    f <- fs::path(d, "f.R")
    writeLines(paste0("v", i), f)
    Sys.setFileTime(f, fixed_mtime)   # identical file mtimes -> old code collapses to 1 row
  }
  fs::dir_create(fs::path(root, "sub", "deep")); writeLines("live", fs::path(root, "sub", "deep", "f.R"))

  src <- new_source("zfs", list(dataset_root = root))     # preset defaults: glob + time_from
  snaps <- src$list_snapshots(fs::path(root, "sub", "deep", "f.R"))
  expect_equal(nrow(snaps), 4)                            # ALL snapshots, no collapse
  expect_equal(snaps$label, snap_names)                   # datetime order matches
  expect_true(all(diff(as.numeric(snaps$time)) > 0))      # strictly increasing
  # id is the snapshot dir; read_file reconstructs and reads the right version
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
