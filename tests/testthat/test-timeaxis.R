test_that("time_axis_ticks adapts label granularity to span", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  # ~1 week -> day labels like "May 26"
  tk <- time_axis_ticks(utc("2026-05-25 00:00:00"), utc("2026-06-01 00:00:00"))
  expect_s3_class(tk, "tbl_df")
  expect_true(nrow(tk) >= 2)
  expect_true(all(tk$time >= utc("2026-05-25 00:00:00") & tk$time <= utc("2026-06-01 00:00:00")))
  expect_true(any(grepl("^[A-Z][a-z]{2} [0-9]{2}$", tk$label)))     # "May 26"
  # ~1 year -> month+year labels like "Mar 2026"
  tk2 <- time_axis_ticks(utc("2026-01-01 00:00:00"), utc("2026-12-31 00:00:00"))
  expect_true(any(grepl("^[A-Z][a-z]{2} 2026$", tk2$label)))
  # ~1 day -> hour labels like "06:00"
  tk3 <- time_axis_ticks(utc("2026-05-31 00:00:00"), utc("2026-05-31 23:59:00"))
  expect_true(any(grepl("^[0-9]{2}:[0-9]{2}$", tk3$label)))
})

test_that("time_axis_ticks handles a degenerate (zero) span", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  tk <- time_axis_ticks(utc("2026-05-31 09:00:00"), utc("2026-05-31 09:00:00"))
  expect_equal(nrow(tk), 1)
})

test_that("time_positions maps within range and clamps out-of-range", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  from <- utc("2026-01-01 00:00:00"); to <- utc("2026-01-11 00:00:00")
  p <- time_positions(c(from, utc("2026-01-06 00:00:00"), to), from, to)
  expect_equal(round(p), c(0, 50, 100))
  expect_equal(time_positions(utc("2025-01-01 00:00:00"), from, to), 0)  # clamp low
  expect_equal(time_positions(utc("2027-01-01 00:00:00"), from, to), 100) # clamp high
})

test_that("time_positions returns centered values for zero span", {
  t <- as.POSIXct("2026-05-31 09:00:00", tz = "UTC")
  expect_equal(time_positions(c(t, t), t, t), c(50, 50))
})
