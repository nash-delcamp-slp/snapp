test_that("SnapshotSource abstract methods stop()", {
  s <- SnapshotSource$new(name = "x")
  expect_equal(s$name, "x")
  expect_error(s$list_snapshots("/f"), "abstract")
  expect_error(s$read_file("/f", "1"), "abstract")
})

test_that("SnapshotSource root/list_children are abstract", {
  s <- SnapshotSource$new(name = "x")
  expect_error(s$root(), "abstract")
  expect_error(s$list_children(), "abstract")
})

test_that("classify distinguishes text, image, binary", {
  s <- SnapshotSource$new(name = "x")
  expect_equal(s$classify(charToRaw("a,b\n1,2"), "data.csv"), "text")
  expect_equal(s$classify(as.raw(c(0x89, 0x50)), "logo.PNG"), "image")
  expect_equal(s$classify(as.raw(c(0x00, 0x01, 0x02)), "blob.bin"), "binary")
})
