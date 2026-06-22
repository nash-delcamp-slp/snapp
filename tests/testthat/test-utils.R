test_that("%||% returns lhs unless NULL", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
})

test_that("is_probably_text detects NUL bytes as binary", {
  expect_true(is_probably_text(charToRaw("hello\nworld")))
  expect_false(is_probably_text(as.raw(c(0x68, 0x00, 0x69))))
  expect_true(is_probably_text(raw(0)))  # empty = text
})
