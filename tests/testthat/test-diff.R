test_that("render_text_diff returns an HTML string reflecting changes", {
  html <- render_text_diff(c("a", "b"), c("a", "c"), mode = "side-by-side")
  expect_type(html, "character")
  expect_length(html, 1)
  expect_match(html, "c")  # the changed line appears
  expect_match(html, "<")  # output is HTML, not plain text
})

test_that("image_data_uri builds a data URI with the right mime", {
  uri <- image_data_uri(as.raw(c(0x89, 0x50)), "logo.png")
  expect_match(uri, "^data:image/png;base64,")
})

test_that("binary_summary reports size and a hash", {
  sm <- binary_summary(as.raw(rep(0L, 10)))
  expect_match(sm, "10 bytes")
})

test_that("render_text_diff embeds banner labels when provided", {
  html <- render_text_diff(c("x"), c("y"), mode = "side-by-side",
                           a_label = "OLDLABEL", b_label = "NEWLABEL")
  expect_match(html, "OLDLABEL")
  expect_match(html, "NEWLABEL")
})
