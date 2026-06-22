test_that("paths_to_tree builds nested structure with file-typed leaves", {
  tr <- paths_to_tree(c("/p/a.R", "/p/sub/b.R"))
  expect_true("p" %in% names(tr))
  expect_true(all(c("a.R", "sub") %in% names(tr$p)))
  expect_true("b.R" %in% names(tr$p$sub))
  expect_equal(attr(tr$p$a.R, "sttype"), "file")
  expect_equal(attr(tr$p$sub$b.R, "sttype"), "file")
})

test_that("paths_to_tree merges siblings under a shared directory", {
  tr <- paths_to_tree(c("/p/a.R", "/p/b.R"))
  expect_true(all(c("a.R", "b.R") %in% names(tr$p)))
})

test_that("paths_to_tree handles a single top-level file", {
  tr <- paths_to_tree("/x.R")
  expect_true("x.R" %in% names(tr))
  expect_equal(attr(tr$x.R, "sttype"), "file")
})
