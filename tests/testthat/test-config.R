test_that("default_config has expected shape", {
  cfg <- default_config()
  expect_equal(cfg$settings$default_view, "side-by-side")
  expect_true(cfg$settings$auto_discover)
  expect_equal(cfg$sources, list())
})

test_that("validate_config rejects bad structures", {
  expect_error(validate_config(list(settings = list(default_view = "x"), sources = list())),
               "default_view")
  expect_error(validate_config(list(settings = list(default_view = "unified"),
                                     sources = list(list(type = "git")))),
               "name")
})

test_that("read_config creates a default when missing, write/read round-trips", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "config.yml")
  cfg <- read_config(p)               # file absent -> writes default
  expect_true(file.exists(p))
  expect_equal(cfg$settings$default_view, "side-by-side")

  cfg$sources <- list(list(name = "g", type = "git", enabled = TRUE,
                           params = list(repo = "/r")))
  write_config(cfg, p)
  again <- read_config(p)
  expect_equal(again$sources[[1]]$name, "g")
  expect_equal(again$sources[[1]]$params$repo, "/r")
})
