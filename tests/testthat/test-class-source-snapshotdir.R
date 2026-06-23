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
