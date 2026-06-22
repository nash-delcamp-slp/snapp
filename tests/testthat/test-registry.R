test_that("register/new/source_types round-trip", {
  local_registry()  # helper below resets registry per test
  register_source_type("fake", new = function(params) {
    do.call(FakeSource$new, params)
  })
  expect_true("fake" %in% source_types())
  s <- new_source("fake", list(name = "f1"))
  expect_s3_class(s, "FakeSource")
  expect_equal(s$name, "f1")
})

test_that("new_source errors on unknown type", {
  local_registry()
  expect_error(new_source("nope", list()), "Unknown source type")
})

test_that("discover_sources merges and dedupes detector output", {
  local_registry()
  register_source_type("a", new = identity,
    detect = function(path) list(list(name = "A", type = "a", params = list(p = 1))))
  register_source_type("b", new = identity,
    detect = function(path) list(list(name = "B", type = "b", params = list(p = 2))))
  register_source_type("c", new = identity, detect = NULL)
  got <- discover_sources("/some/path")
  expect_equal(length(got), 2)
  expect_setequal(vapply(got, `[[`, character(1), "type"), c("a", "b"))
})
