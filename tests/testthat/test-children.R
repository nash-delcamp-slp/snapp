test_that("merge_children unions immediate children across sources, dirs first", {
  a <- FakeSource$new(name="a", root="/p", data=list("/p/x.R"=1, "/p/sub/b.R"=1))
  b <- FakeSource$new(name="b", root="/p", data=list("/p/y.R"=1, "/p/sub/c.R"=1))
  kids <- merge_children(list(a,b), "/p")
  expect_setequal(kids$name, c("sub","x.R","y.R"))
  expect_equal(kids$type[kids$name=="sub"], "dir")
  expect_equal(kids$name[1], "sub")        # dirs sorted before files
})

test_that("find_files does a bounded recursive search by name substring", {
  a <- FakeSource$new(name="a", root="/p",
    data=list("/p/model.R"=1, "/p/sub/model_helpers.R"=1, "/p/sub/other.txt"=1))
  hits <- find_files(list(a), "/p", "model")
  expect_true(all(grepl("model", tolower(hits$name))))
  expect_setequal(basename(hits$path), c("model.R","model_helpers.R"))
})

test_that("find_files respects max_results cap", {
  files <- stats::setNames(as.list(rep(1, 10)), sprintf("/p/match_%02d.R", 1:10))
  a <- FakeSource$new(name="a", root="/p", data=files)
  hits <- find_files(list(a), "/p", "match", max_results = 3L)
  expect_equal(nrow(hits), 3L)
})

test_that("merge_children isolates a failing source and warns", {
  good <- FakeSource$new(name = "good", root = "/p", data = list("/p/x.R" = 1))
  bad  <- FakeSource$new(name = "bad",  root = "/p", fail = TRUE)
  expect_warning(kids <- merge_children(list(good, bad), "/p"),
                 class = "snapp_source_error")
  expect_true("x.R" %in% kids$name)        # surviving source still listed
})
